from pydantic import BaseModel
from pydantic.dataclasses import dataclass
import json

import dataclasses


@dataclass(frozen=True)
class Hallo:
    niko: str
    test: str = "Niko"


class _RootTest(BaseModel):
    __root__: Hallo


if __name__ == '__main__':
    json = json.dumps({
        "niko": "Halo"
    })

    d = _RootTest.parse_raw(json).__root__
    print(d)
    ddd = dataclasses.replace(d, test="tester")
    print(ddd)

