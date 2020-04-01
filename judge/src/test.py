from pydantic.dataclasses import dataclass
import json


@dataclass(frozen=True)
class Hallo:
    niko: str
    test: str = "Niko"


if __name__ == '__main__':
    json = json.dumps({
        "niko": "Halo"
    })

    d = Hallo.__pydantic_model__.parse_raw(json)
    print(d)
    dd = d.copy(update={"test": "2"})
    print(dd)

