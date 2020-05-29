@dataclass
class FunctionCall:
    type: FunctionType
    name: str
    namespace: Optional[str] = None
    arguments: List[Union['Expression', NamedArgument]] = field(default_factory=list)
