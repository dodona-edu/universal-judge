import json
import os

from scipy.stats import linregress
import matplotlib.pyplot as plt
from matplotlib.ticker import MaxNLocator

# from global_alignment_local_exercise import GlobalAlignmentLocal as Exercise
# from global_alignment_local_exercise_controlled import GlobalAlignmentLocalControlled as Exercise
# from log_search_exercise import LogSearch as Exercise
from stable_marriage_exercise import StableMarriage as Exercise

def main():
    exercise = Exercise()
    results = {}

    result_files = os.listdir(os.path.join('results', exercise.name))
    users = {f.replace('-memory.txt', '').replace('-time.txt', '') for f in result_files}
    for user in sorted(users):
        res = []
        with open(os.path.join('results', exercise.name, f'{user}-time.txt')) as infile:
            res.append(json.load(infile))
        with open(os.path.join('results', exercise.name, f'{user}-memory.txt')) as infile:
            res.append(json.load(infile))
        results[user] = res

    images_dir = os.path.join('images', exercise.name)
    os.makedirs(images_dir, exist_ok=True)

    for user in results:
        confidences = [
            [[] for _ in exercise.symbolic_time_complexity],
            [[] for _ in exercise.symbolic_memory_complexity],
        ]
        for i in range(2, len(results[user][0][1]) + 1):
            for j, xs in enumerate(results[user][0][0]):
                confidences[0][j].append(linregress(xs[:i], results[user][0][1][:i])[2])
            for j, xs in enumerate(results[user][1][0]):
                confidences[1][j].append(linregress(xs[:i], results[user][1][1][:i])[2])

        fig, ax = plt.subplots()
        ax.hlines([0.8], 0, 1, transform=ax.get_yaxis_transform(), colors='gray', linestyles='dashed')
        for i, ys in enumerate(confidences[0]):
            ax.plot(list(range(3, len(ys) + 3)), ys, label=f'O({exercise.symbolic_time_complexity[i]})')
        ax.set_ylim(-1.05, 1.05)
        ax.set_xlim(left=2.5)
        ax.xaxis.set_major_locator(MaxNLocator(integer=True))
        ax.legend()
        plt.xlabel('# Uitgevoerde testen')
        plt.ylabel('Correlatiecoëfficient')
        fig.savefig(os.path.join(images_dir, f'{user}-time-confidence.png'))
        plt.close(fig)

        fig, ax = plt.subplots()
        ax.plot(list(range(1, len(results[user][0][1]) + 1)), results[user][0][1])
        plt.xlabel('Test')
        plt.ylabel('Uitvoeringstijd (s)')
        fig.savefig(os.path.join(images_dir, f'{user}-time.png'))
        plt.close(fig)

        fig, ax = plt.subplots()
        ax.hlines([0.8], 0, 1, transform=ax.get_yaxis_transform(), colors='gray', linestyles='dashed')
        for i, ys in enumerate(confidences[1]):
            ax.plot(list(range(3, len(ys) + 3)), ys, label=f'O({exercise.symbolic_memory_complexity[i]})')
        ax.set_ylim(-1.05, 1.05)
        ax.set_xlim(left=2.5)
        ax.xaxis.set_major_locator(MaxNLocator(integer=True))
        ax.legend()
        plt.xlabel('# Uitgevoerde testen')
        plt.ylabel('Correlatiecoëfficient')
        fig.savefig(os.path.join(images_dir, f'{user}-memory-confidence.png'))
        plt.close(fig)

        fig, ax = plt.subplots()
        ax.plot(list(range(1, len(results[user][1][1]) + 1)), [x / 1024 for x in results[user][1][1]])
        ax.xaxis.set_major_locator(MaxNLocator(integer=True))
        plt.xlabel('Test')
        plt.ylabel('Geheugenverbruik (kB)')
        fig.savefig(os.path.join(images_dir, f'{user}-memory.png'))
        plt.close(fig)


if __name__ == '__main__':
    main()
