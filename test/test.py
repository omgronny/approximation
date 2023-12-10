import os

def test(name, input_file, output_file, method):
    stream = os.popen(f"cat test/{input_file} | ./interpolate  --step=0.5 --window=3 --method={method}")
    res = stream.readlines()
    file = open(f"test/{output_file}", "r")
    excepted = file.readlines()

    for i in range(len(excepted)):
        res_i = res[i].split(":")
        excepted_i = excepted[i].split(":")
        assert res_i[-1] == excepted_i[-1]

    print(f"{name} passed")

test("test1 linear", "test1", "test1_output", "linear")
test("test2 linear", "test2", "test2_output_lin", "linear")

test("test1 lagrangia", "test1", "test1_output", "lagrangia")
test("test2 lagrangia", "test2", "test2_output_lagr", "lagrangia")
