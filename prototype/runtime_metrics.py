import time

class LineWatcher(object):

    """Class that implements a basic timer.

    Notes
    -----
    * Register the `start` and `stop` methods with the IPython events API.
    """

    def __init__(self):
        
        self.start_time = 0.0

    def start(self):
        
        self.start_time = time.time()

    def stop(self):
        if self.start_time:
            print('time: {}s'.format(time.time() - self.start_time))

timer = LineWatcher()

def load_ipython_extension(ip):
    ip.events.register('pre_execute', timer.start)
    ip.events.register('post_execute', timer.stop)

def unload_ipython_extension(ip):
    ip.events.unregister('pre_execute', timer.start)
    ip.events.unregister('post_execute', timer.stop)
    