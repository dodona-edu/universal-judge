import java.io.IOException;
import java.util.List;

abstract class AbstractCustomEvaluator extends AbstractEvaluator {

    abstract void evaluate(Object expected,
                           Object actual,
                           List<Object> arguments) throws IOException;
}
