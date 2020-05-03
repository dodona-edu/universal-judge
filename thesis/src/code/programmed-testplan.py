@dataclass
class ProgrammedEvaluator:
    language: str
    function: EvaluationFunction
    arguments: List[Value] = field(default_factory=list)
    type: Literal['programmed'] = 'programmed'
