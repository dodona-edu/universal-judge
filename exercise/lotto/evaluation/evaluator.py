# noinspection DuplicatedCode
def evaluate(expected, actual):
    # noinspection DuplicatedCode
    def valid_lottery_numbers(numbers='', count=6, maximum=42):

        def listing(numbers):
            """
            Creates string representation of a given list of integers
            """
            if len(numbers) == 1:
                return str(numbers[0])
            else:
                return f"{', '.join(str(x) for x in numbers[:-1])} en {numbers[-1]}"

        import re

        if not isinstance(numbers, str):
            return numbers, False, 'lottogetallen moet een string zijn'

        if not re.match('^(-?(0|-?[1-9][0-9]*) - )*(0|-?[1-9][0-9]*)$', numbers):
            return numbers, False, 'lottogetallen worden niet in het correcte formaat teruggegeven'

        number_list = list(int(x) for x in numbers.split(' - '))

        if len(number_list) != count:
            return numbers, False, f"verwachtte {count} in plaats van {len(number_list)} lottogetallen"

        wrong = [number for number in number_list if number > maximum]
        if wrong:
            return (
                numbers, False,
                f'volgende lottogetallen zijn groter dan de maximale waarde {maximum}: {listing(wrong)}'
            )

        wrong = [number for number in number_list if number < 1]
        if wrong:
            return numbers, False, f'volgende lottogetallen zijn kleiner dan de minimale waarde 1: {listing(wrong)}'

        wrong = list(sorted({number for number in number_list if number_list.count(number) > 1}))
        if wrong:
            return numbers, False, f'volgende lottogetallen komen meer dan één keer voor: {listing(wrong)}'

        if list(sorted(number_list)) != number_list:
            return numbers, False, 'lottogetallen worden niet in stijgende volgorde opgelijst'

        return numbers, True, ''

    expected_value = expected[0]
    count = expected[1]
    maximum = expected[2]

    # at least check the value that was already generated
    numbers, valid, message = valid_lottery_numbers(actual, count=count, maximum=maximum)

    messages = []
    if message:
        messages.append('Fout: ' + message)

    evaluated(valid, expected_value, messages=messages)
