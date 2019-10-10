import atexit
import queue
from threading import Thread

import time
import zmq
from jupyter_client.manager import start_new_kernel

from utils.memory_limit import MemoryLimit
from utils.timeout import Timeout


class JupyterContext(Thread):
    def __init__(self, language='python3', allow_stdin=True):
        super().__init__()
        self.language = language
        self.allow_stdin = allow_stdin
        self.failed = False

    def run(self):
        self.manager, self.client = start_new_kernel(kernel_name=self.language)
        atexit.register(self.clean)
        self.client.allow_stdin = self.allow_stdin
        # for elem in self.user_code:
        #     self.execute_statements(elem)
        return self

    def clean(self):
        atexit.unregister(self.clean)
        self.client.stop_channels()
        self.manager.shutdown_kernel()

    def restart(self):
        self.manager.restart_kernel(now=True)

    def is_running(self):
        return self.manager.is_alive()

    def execute_statements(self, statements, timeout, memory_limit, std_input=None, silent=False):
        self.__flush_channels()
        kernel_finished, unprocessed_messages = False, True
        std_input = std_input.splitlines(False) if std_input is not None else []
        with Timeout(timeout) as timeout:
            with MemoryLimit(memory_limit, self.manager.kernel.pid) as memory_limiter:
                start_time = time.perf_counter()
                self.client.execute(code=statements, silent=silent, store_history=False)

                messages = {'iopub': [], 'stdin': [], 'client': []}

                while not kernel_finished or unprocessed_messages:
                    if timeout.expired:
                        messages['client'].append({
                            'msg_type': 'error',
                            'content': {
                                'ename': 'TimeoutError',
                                'evalue': 'time limit exceeded'
                            }
                        })
                        return messages

                    if memory_limiter.exceeded:
                        messages['client'].append({
                            'msg_type': 'error',
                            'content': {
                                'ename': 'MemoryError',
                                'evalue': 'memory limit exceeded'
                            }
                        })
                        return messages

                    if not unprocessed_messages:
                        zmq.select([self.client.iopub_channel.socket, self.client.stdin_channel.socket], [], [],
                                   timeout=1)
                    unprocessed_messages = False

                    for channel in (
                            self.client.iopub_channel,
                            self.client.stdin_channel,
                    ):
                        try:
                            message = channel.get_msg(block=False)
                            unprocessed_messages = True

                            if channel is self.client.iopub_channel:
                                if message['msg_type'] == 'status':
                                    if message['content']['execution_state'] == 'idle':
                                        kernel_finished = True
                                    elif message['content']['execution_state'] in {'starting'}:
                                        pass
                                    elif message['content']['execution_state'] in {'busy'}:
                                        start_time = time.perf_counter()
                                elif message['msg_type'] == 'execute_input':
                                    pass
                                else:
                                    messages['iopub'].append(message)
                            elif message['msg_type'] == 'input_request':
                                if std_input:
                                    self.client.input(std_input.pop(0))
                                else:
                                    messages['client'].append({
                                        'msg_type': 'error',
                                        'content': {
                                            'ename': 'TooMuchInput',
                                            'evalue': 'EOF while running code'
                                        }
                                    })
                                    kernel_finished = True
                            else:
                                messages['stdin'].append(message)
                        except queue.Empty:
                            pass
                return messages

    def __flush_channels(self):
        for channel in (
                self.client.shell_channel,
                self.client.iopub_channel,
                self.client.stdin_channel,
        ):
            while True:
                try:
                    _ = channel.get_msg(block=False)
                except queue.Empty:
                    break


class RunError(RuntimeError):
    def __init__(self, error):
        self.error = error


FAST_KERNELS = {
    'python': '%reset -f in out dhist'
}


class KernelQueue:
    """
    A multithreaded kernel manager. This allows clients to execute multiple statements after each other, without waiting
    for start and restart times.
    The first time you create a new instance of this class, one kernel is created synchronously.
    """

    def __init__(self, language, size=None):
        self.language = language
        self.queue = queue.Queue()
        if size is None:
            self.size = 1 if language in FAST_KERNELS else 4
        else:
            self.size = size
        self.stopping = False

        # Create first one synchronously.
        self._fill(self.size)

    def _fill(self, amount):
        while self.queue.qsize() < amount:
            self._add_new_kernel()

    def new_kernel(self):
        # print(f"Producing new kernel")
        return JupyterContext(self.language)

    def _add_new_kernel(self):
        kernel = self.new_kernel()
        kernel.run()
        if self.stopping:
            kernel.clean()
            # print("Ignoring reset kernel, since we are stopping")
        else:
            self.queue.put(kernel)
            # print("Kernel was constructed and added to the queue!")

    def get_kernel(self, existing=None):
        # print(f"Getting kernel from queue with size {self.queue.qsize()}")
        new_kernel = self.queue.get()
        if existing is not None:
            # print(f"Re-purposing existing kernel")
            t = Thread(name="KR", target=self._reset_kernel, args=[existing])
            t.setDaemon(True)
            t.start()
        # print("Returning new kernel")
        return new_kernel

    def _reset_kernel(self, kernel: JupyterContext):
        if self.language in FAST_KERNELS:
            # print("Doing fast reset")
            r = kernel.execute_statements(FAST_KERNELS[self.language], 1, None)
            if not any(m['msg_type'] == 'error' for m in r['client']):
                if self.stopping:
                    kernel.clean()
                    # print("Ignoring reset kernel, since we are stopping")
                else:
                    self.queue.put(kernel)
                    # print("Kernel was reset and added back to the queue!")
                return

        # print("Doing slow kernel reset...")
        kernel.clean()
        new_kernel = self.new_kernel()
        new_kernel.run()
        if self.stopping:
            new_kernel.clean()
            # print("Ignoring reset kernel, since we are stopping")
        else:
            self.queue.put(new_kernel)
            # print("Kernel was reset and added back to the queue!")

    def clean(self):
        self.stopping = True
        while not self.queue.empty():
            k = self.queue.get()
            k.clean()
