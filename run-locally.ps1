$data = @{
    memory_limit = 536870912;
    time_limit = 10000;
    programming_language = 'python';
    natural_language = 'nl';
    resources = './excercise';
    source = './excercise/test.py';
    judge = 'ignored';
    workdir = 'ignored';
}

ConvertTo-Json $data | python ./tested.py
#Measure-Command { ConvertTo-Json $data | python ./tested.py | Out-Default}