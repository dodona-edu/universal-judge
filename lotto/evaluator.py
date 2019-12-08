def evaluate(expected, actual):

    def valid_lottery_numbers(numbers='', count=6, maximum=42):

        def listing(numbers):
            """
            Creates string representation of a given list of integers
            """
            if len(numbers) == 1:
                return str(numbers[0])
            else:
                return '{} and {}'.format(
                    ', '.join(str(x) for x in numbers[:-1]),
                    numbers[-1]
                )

        import re

        if isinstance(numbers, str):
            lottery_numbers = numbers
        else:
            return (
                numbers,
                False,
                'lottery numbers must be a string'
            )

        if not re.match('^(-?(0|-?[1-9][0-9]*) - )*(0|-?[1-9][0-9]*)$', lottery_numbers):
            return (
                lottery_numbers,
                False,
                'lottery numbers are not listed in the correct format'
            )

        number_list = list(int(x) for x in lottery_numbers.split(' - '))

        if len(number_list) != count:
            return (
                lottery_numbers,
                False,
                'expected {} instead of {} lottery numbers'.format(
                    count, len(number_list)
                )
            )

        wrong = [number for number in number_list if number > maximum]
        if wrong:
            return lottery_numbers, False, 'the following lottery numbers exceed the maximum value of {}: {}'.format(
                maximum, listing(wrong))

        wrong = [number for number in number_list if number < 1]
        if wrong:
            return lottery_numbers, False, 'the following lottery numbers are smaller than the minimum value of 1: {}'.format(
                listing(wrong))

        wrong = list(sorted({number for number in number_list if number_list.count(number) > 1}))
        if wrong:
            return lottery_numbers, False, 'the following lottery numbers occur more than once: {}'.format(
                listing(wrong))

        if list(sorted(number_list)) != number_list:
            return lottery_numbers, False, 'lottery numbers are not listed in increasing order'

        return lottery_numbers, True, ''

    expected_value = expected[0]
    count = expected[1]
    maximum = expected[2]

    # at least check the value that was already generated
    numbers, valid, message = valid_lottery_numbers(actual, count=count, maximum=maximum)

    messages = []
    if message:
        messages.append('Error: ' + message)

    evaluated(valid, expected_value, messages=messages)
