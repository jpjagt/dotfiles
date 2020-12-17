import re
import sys


def overleaf_to_ans(overleaf):
    # one loop
    for pattern, replace in {
        r"\\begin{align\*}": r"\\[ \\begin{aligned}",
        r"\\end{align\*}": r"\\end{aligned} \\]",
        r" {2,}": r" ",
        "\$\$": "XABCDEFG",
        "\$": r"$$",
        "XABCDEFG": r"$$$",
        "\\n": r" ",
    }.items():
        overleaf = re.sub(pattern, replace, overleaf)

    # multiple loops
    for pattern, replace in {
        # r"(\\\[((?!\\[\[\]])[\S\s])*)\n(((?!\\[\[\]])[\S\s])*\\\])": r"\g<1> \g<3>"
    }.items():
        while re.search(pattern, overleaf):
            overleaf = re.sub(pattern, replace, overleaf)
    return overleaf


if __name__ == "__main__":
    with open(sys.argv[1], "r") as f:
        overleaf = f.read()
    print(overleaf_to_ans(overleaf))
