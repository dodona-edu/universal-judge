import os

from dodona_exercise import DodonaExercise
import random


class StableMarriage(DodonaExercise):
    def __init__(self):
        self.test_num = 0

    @property
    def name(self):
        return 'stable_marriage'

    def get_function(self):
        return [
            '''
public class TestData {{
    public static int[][] men;
    public static int[][] women;
}}
            ''',
            '''
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;
            
int[][] men;
int[][] women;
try (Scanner scanner = new Scanner(new File({0}))) {{
    int num = scanner.nextInt();
    men = new int[num][num];
    women = new int[num][num];
    for (int i = 0; i < num; i++) {{
        for (int j = 0; j < num; j++) {{
            men[i][j] = scanner.nextInt();
        }}
    }}

    for (int i = 0; i < num; i++) {{
        for (int j = 0; j < num; j++) {{
            women[i][j] = scanner.nextInt();
        }}
    }}
}} catch (FileNotFoundException ignored) {{
    men = new int[][]{{{{0}}}};
    women = new int[][]{{{{0}}}};
}}

TestData.men = men;
TestData.women = women;
            ''',
            'new StableDatingService().match(TestData.men, TestData.women);'
        ]

    @property
    def initial_parameters(self):
        return 200,

    def get_test(self, parameters):
        num_persons = parameters[0]
        men = [[0] * num_persons for _ in range(num_persons)]
        women = [[0] * num_persons for _ in range(num_persons)]
        for i in range(1, num_persons):
            for j in range(1, num_persons):
                if (i + j - 2) % (num_persons - 1) != 0:
                    men[i - 1][j - 1] = ((i + j - 2) % (num_persons - 1)) - 1
                else:
                    men[i - 1][j - 1] = num_persons - 2
            men[i - 1][num_persons - 1] = num_persons - 1
        for j in range(1, num_persons + 1):
            men[num_persons - 1][j - 1] = men[0][j - 1]

        for i in range(1, num_persons - 1):
            for j in range(1, num_persons + 1):
                if (i + j + 1) % num_persons != 0:
                    women[i - 1][j - 1] = ((i + j + 1) % num_persons) - 1
                else:
                    women[i - 1][j - 1] = num_persons - 1
        for j in range(1, num_persons + 1):
            if (j + 1) % num_persons != 0:
                women[-2][j - 1] = ((j + 1) % num_persons) - 1
            else:
                women[-2][j - 1] = num_persons - 1
            women[-1][j - 1] = j - 1

        filename = os.path.join('data', f'data{self.test_num}.stableMarriage')
        self.test_num += 1
        with open(filename, 'w') as testfile:
            print(len(men), file=testfile)
            print(file=testfile)
            for m in men:
                print(' '.join(str(x) for x in m), file=testfile)
            print(file=testfile)
            for w in women:
                print(' '.join(str(x) for x in w), file=testfile)

        return f'"{filename}"',

    @property
    def symbolic_time_complexity(self):
        return 'n^2', 'n^3'

    def time_complexity(self, args):
        return args[0] ** 2, args[0] ** 3

    @property
    def symbolic_memory_complexity(self):
        return 'n^2', 'n'

    def memory_complexity(self, args):
        return args[0] ** 2, args[0]

    @property
    def language(self):
        return 'java'

    @property
    def submissions_url(self):
        return 'https://dodona.ugent.be/nl/courses/59/exercises/946407776/submissions/'

    @property
    def preload_code(self):
        return '''public interface DatingService {

    public int[] match(int[][] men, int[][] women);

}'''
