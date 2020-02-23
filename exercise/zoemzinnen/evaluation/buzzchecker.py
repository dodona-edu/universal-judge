import html
import evaluation_utils


def ordinal(index):
    if index == 0:
        return 'first'
    elif index == 1:
        return 'second'
    elif index == 2:
        return 'third'
    else:
        return '{}th'.format(index + 1)


def highlight(words, index=-1):
    def hl(word, index1, index2):
        return '<span>{}</span>'.format(word) if index1 == index2 else word

    return ' '.join(hl(html.escape(w), i, index) for i, w in enumerate(words))


def evaluate(expected, actual, arguments):
    """
    Checks whether a buzz-generated generators always produces a string that is a
    random selection of the words from the given word lists.

    :param expected: The expected output from the testplan.
    :param actual: The actual output from the testplan.
    :param arguments: A list with one element: the input.
    :return:
    """
    wordlists = arguments[0]
    wordlists = [set() for _ in range(len(wordlists))]

    words = actual.split()
    # check if actual has same number of words as there are word lists
    if len(words) != len(wordlists):
        evaluation_utils.evaluated(
            result=False,
            readable_expected=repr(expected),
            readable_actual=repr(actual),
            messages=[f"buzz-actual should contain {len(wordlists)} words"]
        )
        return

    # check if each selected word belongs to the corresponding word list
    for index, word in enumerate(words):
        if word not in wordlists[index]:
            message = f'"{word} does not belong to {ordinal(index)} word list"'
            evaluation_utils.evaluated(
                result=False,
                readable_expected=html.escape(repr(expected)),
                readable_actual=repr(highlight(words, index)),
                messages=[message]
            )
            return
        else:
            wordlists[index].add(word)

    if actual != ' '.join(words):
        message = ('words must be separated by a single space, without leading or '
                   'trailing whitespace')

        evaluation_utils.evaluated(
            result=False,
            readable_expected=html.escape(' '.join(words)),
            readable_actual=html.escape(actual),
            messages=[message]
        )
        return

    evaluation_utils.evaluated(
        result=True,
        readable_expected=html.escape(actual),
        readable_actual=html.escape(actual)
    )
