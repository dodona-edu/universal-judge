from .parser import *

with Judgment() as judgment:
    with Tab(judgment, "Correctheid") as tab:
        with Context(tab) as context:
            with Testcase(context, "test") as testcase:
                with Test(testcase) as test:
                    test.description("Een mooie test")
                    test.stdin("Hallo")
                    test.stdout("Hallo2")


