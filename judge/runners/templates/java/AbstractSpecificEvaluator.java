import java.io.IOException;
import java.util.List;

abstract class AbstractSpecificEvaluator extends AbstractEvaluator {

    abstract void evaluate(Object actual,
                           List<Object> arguments) throws IOException;
}
