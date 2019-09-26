import itertools
import json
import math
import os
import time

from scipy.stats import linregress

from jupyter import run_outside as run, RunError, get_queue, fill_queue, clean_queue
# from log_search_exercise import LogSearch as Exercise
# from global_alignment_exercise import GlobalAlignment as Exercise
from stable_marriage_exercise import StableMarriage as Exercise
# from global_alignment_local_exercise import GlobalAlignmentLocal as Exercise
# from global_alignment_local_exercise_controlled import GlobalAlignmentLocalControlled as Exercise

TIME_PER_SUBMISSION = 1200
TIMES = 3
ALLOW_EMPTY = False
REQUIRED_CONFIDENCE = .8


def regressions(results):
    if len(results[0][1]) == 1:
        return [
            [(results[i][1][0] / results[i][0][j][0], 0, 1, None, None)
             for j in range(len(results[i][0]))]
            for i in range(len(results))
        ]
    else:
        return [
            [linregress(results[i][0][j], results[i][1])
             for j in range(len(results[i][0]))]
            for i in range(len(results))]


def pairwise_uncorrelated(exercise, results, regressions, res_args):
    total = 0
    for (j, k) in itertools.combinations(range(len(regressions[0])), r=2):
        expected_times = [exercise.time_complexity(args) for args in res_args]
        cond = linregress(
            results[0][1] + [regressions[0][j][0] * expected_time[j] + regressions[0][j][1]
                             for expected_time in expected_times],
            results[0][1] + [regressions[0][k][0] * expected_time[k] + regressions[0][k][1]
                             for expected_time in expected_times]
        )[2] < REQUIRED_CONFIDENCE
        if cond:
            total += 1
    for (j, k) in itertools.combinations(range(len(regressions[1])), r=2):
        expected_memories = [exercise.memory_complexity(args) for args in res_args]
        if linregress(
                results[1][1] + [regressions[1][j][0] * expected_memory[j] + regressions[1][j][1]
                                 for expected_memory in expected_memories],
                results[1][1] + [regressions[1][k][0] * expected_memory[k] + regressions[1][k][1]
                                 for expected_memory in expected_memories]
        )[2] < REQUIRED_CONFIDENCE:
            total += 1
    return total


def nc2(n):
    f = math.factorial
    if n - 2 < 0:
        return 0
    return f(n) // f(2) // f(n - 2)


def next_args(exercise, complexity_args, results, remaining_time):
    regs = regressions(results)
    functions_to_use = [{x for x in range(len(results[i][0]))} for i in range(len(results))]
    for i in range(len(regs)):
        for j, reg in enumerate(regs[i]):
            if reg[2] < REQUIRED_CONFIDENCE:
                functions_to_use[i].remove(j)
    max_args = tuple(max(x[i] for x in complexity_args) for i in range(len(complexity_args[0])))
    iteration = 1
    last_iteration_with_change = 1
    res_args = set()
    current = 0
    while current < nc2(len(regs[0])) + nc2(len(regs[1])) and iteration < last_iteration_with_change + 20:
        for i in range(len(regs)):
            for elem in itertools.product((0, iteration + 1), repeat=len(complexity_args[0])):
                args = tuple(max_args[i] + elem[i] * complexity_args[0][i]
                             for i in range(len(complexity_args[0])))
                if args not in complexity_args:
                    new_uncor = pairwise_uncorrelated(exercise, results, regs, res_args | {args})
                    if new_uncor > current:
                        res_args.add(args)
                        current = new_uncor
                        last_iteration_with_change = iteration
        iteration += 1

    # Remove with too great runtime
    res_args = set(filter(lambda args: all(
        regs[0][j][0] * exercise.time_complexity(args)[j] + regs[0][j][1] < remaining_time
        for j in functions_to_use[0]), res_args))
    current = pairwise_uncorrelated(exercise, results, regs, res_args)

    # Remove unnecessary
    removed = True
    while removed and len(res_args) > 0:
        removed = False
        for test_args in itertools.combinations(res_args, len(res_args) - 1):
            if pairwise_uncorrelated(exercise, results, regs, test_args) == current:
                res_args = set(test_args)
                removed = True
                break

    return (list(res_args) if res_args or ALLOW_EMPTY else
            [tuple(x + y for x, y in zip(max_args, complexity_args[0]))]) * TIMES


def main():
    exercise = Exercise()
    results = {}

    if not os.path.exists(os.path.join('results', exercise.name)):
        os.makedirs(os.path.join('results', exercise.name))

    if not os.path.isdir(os.path.join('results', exercise.name)):
        print(f'{os.path.join("results", exercise.name)} is not a directory')
        exit(1)

    submissions = exercise.get_submissions()
    for j, user in enumerate(submissions):
        full_submission = submissions[user]
        context_queue = get_queue(code=full_submission, language=exercise.language)

        complexity_args = [exercise.initial_parameters]
        all_args = []

        num_time_fns = len(exercise.time_complexity(complexity_args[0]))
        num_mem_fns = len(exercise.memory_complexity(complexity_args[0]))
        results[user] = (
            (tuple([] for _ in range(num_time_fns)), []),
            (tuple([] for _ in range(num_mem_fns)), [])
        )
        start = time.perf_counter()
        try:
            while time.perf_counter() - start < TIME_PER_SUBMISSION and len(complexity_args) > 0:
                all_args.append(complexity_args.pop(0))
                print(all_args[-1])
                test = exercise.get_test(all_args[-1])
                opt_times = exercise.time_complexity(all_args[-1])
                opt_mems = exercise.memory_complexity(all_args[-1])
                queue_element = context_queue.get()
                queue_element.join()
                while queue_element.failed:
                    queue_element = context_queue.get()
                    queue_element.join()
                    fill_queue(context_queue, code=full_submission, language=exercise.language)
                fill_queue(context_queue, code=full_submission, language=exercise.language)
                run_time, max_mem = run(
                    queue_element, exercise.get_function(), test, memory_limit=10 * (1024 ** 3),
                    timeout=TIME_PER_SUBMISSION - (time.perf_counter() - start))
                queue_element.clean()
                for i in range(len(opt_times)):
                    results[user][0][0][i].append(opt_times[i])
                for i in range(len(opt_mems)):
                    results[user][1][0][i].append(opt_mems[i])
                results[user][0][1].append(run_time)
                results[user][1][1].append(max_mem)
                if len(complexity_args) == 0:
                    complexity_args = next_args(exercise, all_args, results[user],
                                                TIME_PER_SUBMISSION - (time.perf_counter() - start))
                    print(complexity_args)
            print(f'Finished {user} ({j + 1}/{len(submissions)}) ({sum(results[user][0][1])} + '
                  f'{(time.perf_counter() - start) - sum(results[user][0][1])})')
        except RunError as e:
            print(f'Finished {user} ({j + 1}/{len(submissions)}) ({sum(results[user][0][1])} + '
                  f'{(time.perf_counter() - start) - sum(results[user][0][1])}) '
                  f'with error in last run: {e.error["evalue"]}')
        with open(os.path.join('results', exercise.name, f'{user}-time.txt'), 'w') as outfile:
            json.dump(results[user][0], outfile)
        with open(os.path.join('results', exercise.name, f'{user}-memory.txt'), 'w') as outfile:
            json.dump(results[user][1], outfile)
        clean_queue(context_queue)

    for user in results:
        if len(results[user][0][1]) > 1:
            print(f'Results for user {user}:')
            for i, symbolic in enumerate(exercise.symbolic_time_complexity):
                xs = results[user][0][0][i]
                ys = results[user][0][1]
                print(
                    f'Time complexity O({symbolic}) with confidence {linregress(xs, ys)[2]}')

            for i, symbolic in enumerate(exercise.symbolic_memory_complexity):
                xs = results[user][1][0][i]
                ys = results[user][1][1]
                print(
                    f'Memory complexity O({symbolic}) with confidence {linregress(xs, ys)[2]}')


if __name__ == '__main__':
    main()
