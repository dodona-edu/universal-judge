from exercise import Exercise
import requests

TOKEN = '60-o0qQGRxLRlckQ5eV4OW_z33aanZEGe5P_32FtKXQ'


def get_correct_submissions(url):
    result = []
    page = 1
    while True:
        latest = requests.get(url, params={'most_recent_correct_per_user': 'true', 'page': str(page)},
                              headers={'Authorization': TOKEN, 'Accept': 'application/json'}).json()
        page += 1
        result.extend(latest)
        if len(latest) == 0:
            return result


def get_submission(url):
    return requests.get(url, headers={'Authorization': TOKEN}).text


class DodonaExercise(Exercise):
    @property
    def name(self):
        raise NotImplementedError('DodonaExercise does not implement name')

    def get_function(self):
        raise NotImplementedError('DodonaExercise does not implement get_function')

    @property
    def initial_parameters(self):
        raise NotImplementedError('DodonaExercise does not implement initial_parameters')

    def get_test(self, parameters):
        raise NotImplementedError('DodonaExercise does not implement get_test')

    @property
    def symbolic_time_complexity(self):
        raise NotImplementedError('DodonaExercise does not implement symbolic_time_complexity')

    def time_complexity(self, args):
        raise NotImplementedError('DodonaExercise does not implement time_complexity')

    @property
    def symbolic_memory_complexity(self):
        raise NotImplementedError('DodonaExercise does not implement symbolic_memory_complexity')

    def memory_complexity(self, args):
        raise NotImplementedError('DodonaExercise does not implement memory_complexity')

    @property
    def language(self):
        raise NotImplementedError('DodonaExercise does not implement language')

    @property
    def submissions_url(self):
        raise NotImplementedError('DodonaExercise does not implement submissions_url')

    @property
    def preload_code(self):
        raise NotImplementedError('DodonaExercise does not implement preload_code')

    def get_submissions(self):
        result = {}
        for elem in get_correct_submissions(self.submissions_url):
            if int(elem["user"]["id"]) < 100:
                result[elem["user"]["id"]] = [self.preload_code, get_submission(elem["url"] + 'download')]
        return result
