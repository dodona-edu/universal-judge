import atexit
import queue
import time
from threading import Thread

import zmq
from jupyter_client.manager import start_new_kernel

from utils.memory_limit import MemoryLimit
from utils.sampler import Sampler
from utils.timeout import Timeout

TIMEOUT = 120
MEMORY_LIMIT = 1000 * 1024 * 1024


def get_queue(code, num_elements=4, language='python3', allow_stdin=True):
    context_queue = queue.Queue()
    for _ in range(num_elements):
        new_element = JupyterContext(language=language, allow_stdin=allow_stdin, code=code)
        new_element.start()
        context_queue.put(new_element)
    return context_queue


def fill_queue(context_queue, code, num_elements=4, language='python3', allow_stdin=True):
    while context_queue.qsize() < num_elements:
        new_element = JupyterContext(language=language, allow_stdin=allow_stdin, code=code)
        new_element.start()
        context_queue.put(new_element)


def clean_queue(context_queue):
    while not context_queue.empty():
        element = context_queue.get()
        element.join()
        element.clean()


class JupyterContext(Thread):
    def __init__(self, language='python3', allow_stdin=True, code=None):
        super().__init__()
        if code is None:
            code = []
        self.language = language
        self.allow_stdin = allow_stdin
        self.code = code
        self.failed = False

    def run(self):
        try:
            self.manager, self.client = start_new_kernel(kernel_name=self.language)
            atexit.register(self.clean)
        except:
            self.failed = True
        self.client.allow_stdin = self.allow_stdin
        for elem in self.code:
            self.execute_statements(elem)
        return self

    def clean(self):
        atexit.unregister(self.clean)
        self.client.stop_channels()
        self.manager.shutdown_kernel()

    def execute_statements(self, statements, timeout=TIMEOUT, memory_limit=MEMORY_LIMIT, std_input=None, silent=False):
        self.__flush_channels()
        kernel_finished, unprocessed_messages = False, True
        with Timeout(timeout) as timeout:
            with MemoryLimit(memory_limit, self.manager.kernel.pid) as memory_limiter:
                with Sampler(self.manager.kernel.pid) as sampler:
                    start_time = time.perf_counter()
                    self.client.execute(code=statements, silent=silent, store_history=False)
                    std_input = std_input.splitlines(False) if std_input is not None else []

                    messages = {'iopub': [], 'stdin': [], 'client': []}

                    total_time = None
                    while not kernel_finished or unprocessed_messages:
                        if timeout.expired:
                            messages['client'].append({
                                'msg_type': 'error',
                                'content': {
                                    'ename': 'TimeoutError',
                                    'evalue': 'time limit exceeded'
                                }
                            })
                            return messages, sampler.samples, time.perf_counter() - start_time

                        if memory_limiter.exceeded:
                            messages['client'].append({
                                'msg_type': 'error',
                                'content': {
                                    'ename': 'MemoryError',
                                    'evalue': 'memory limit exceeded'
                                }
                            })
                            return messages, sampler.samples, time.perf_counter() - start_time

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
                                            if not kernel_finished:
                                                total_time = time.perf_counter() - start_time
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
                                                'ename': 'TimeoutError',
                                                'evalue': 'time limit exceeded'
                                            }
                                        })
                                        if not kernel_finished:
                                            total_time = time.perf_counter() - start_time
                                        kernel_finished = True
                                else:
                                    messages['stdin'].append(message)
                            except queue.Empty:
                                pass
                    total_time = total_time if total_time is not None else time.perf_counter() - start_time
                    return messages, sampler.samples, total_time

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


def run_outside(context, func, args, timeout=None, memory_limit=None):
    for elem in func[:-1]:
        context.execute_statements(elem.format(*args))
    messages, memory_samples, total_time = context.execute_statements(
        func[-1].format(*args),
        timeout=timeout, memory_limit=memory_limit)
    for channel in messages:
        for message in messages[channel]:
            if message['msg_type'] == 'error':
                raise RunError(message['content'])
    return total_time, max(x[1] - min(x[1] for x in memory_samples) for x in memory_samples)
