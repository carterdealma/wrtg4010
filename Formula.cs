// Skeleton written by Profs Zachary, Kopta and Martin for CS 3500
// Read the entire skeleton carefully and completely before you
// do anything else!
// Last updated: August 2023 (small tweak to API)

using System.Text;
using System.Text.RegularExpressions;

namespace SpreadsheetUtilities;

/// <summary>
/// Represents formulas written in standard infix notation using standard precedence
/// rules.  The allowed symbols are non-negative numbers written using double-precision
/// floating-point syntax (without unary preceding '-' or '+');
/// variables that consist of a letter or underscore followed by
/// zero or more letters, underscores, or digits; parentheses; and the four operator
/// symbols +, -, *, and /.
///
/// Spaces are significant only insofar that they delimit tokens.  For example, "xy" is
/// a single variable, "x y" consists of two variables "x" and y; "x23" is a single variable;
/// and "x 23" consists of a variable "x" and a number "23".
///
/// Associated with every formula are two delegates: a normalizer and a validator.  The
/// normalizer is used to convert variables into a canonical form. The validator is used to
/// add extra restrictions on the validity of a variable, beyond the base condition that
/// variables must always be legal: they must consist of a letter or underscore followed
/// by zero or more letters, underscores, or digits.
/// Their use is described in detail in the constructor and method comments.
/// </summary>
public class Formula
{
    private string formulaString;
    private HashSet<string> variables;

    /// <summary>
    /// Creates a Formula from a string that consists of an infix expression written as
    /// described in the class comment.  If the expression is syntactically invalid,
    /// throws a FormulaFormatException with an explanatory Message.
    ///
    /// The associated normalizer is the identity function, and the associated validator
    /// maps every string to true.
    /// </summary>
    /// 
    public Formula(string formula) :
        this(formula, s => s, s => true)
    {
    }

    /// <summary>
    /// Creates a Formula from a string that consists of an infix expression written as
    /// described in the class comment.  If the expression is syntactically incorrect,
    /// throws a FormulaFormatException with an explanatory Message.
    ///
    /// The associated normalizer and validator are the second and third parameters,
    /// respectively.
    ///
    /// If the formula contains a variable v such that normalize(v) is not a legal variable,
    /// throws a FormulaFormatException with an explanatory message.
    ///
    /// If the formula contains a variable v such that isValid(normalize(v)) is false,
    /// throws a FormulaFormatException with an explanatory message.
    ///
    /// Suppose that N is a method that converts all the letters in a string to upper case, and
    /// that V is a method that returns true only if a string consists of one letter followed
    /// by one digit.  Then:
    ///
    /// new Formula("x2+y3", N, V) should succeed
    /// new Formula("x+y3", N, V) should throw an exception, since V(N("x")) is false
    /// new Formula("2x+y3", N, V) should throw an exception, since "2x+y3" is syntactically incorrect.
    /// </summary>
    public Formula(string formula, Func<string, string> normalize, Func<string, bool> isValid)
    {
        List<string> tokenList = GetTokens(formula).ToList();
        int openParenthesisCount = 0;
        int closedParenthesisCount = 0;

        if (tokenList.Count == 0)
        {
            throw new FormulaFormatException("The expression cannot be empty");
        } 

        for (int i = 0; i < tokenList.Count; i++)
        {
            //First token must be a legal variable, legal number, or '('
            if (i == 0 && !tokenList[i].IsLegalVarNumOrOpenParen())
            {
                throw new FormulaFormatException("The expression cannot begin with: " + tokenList[i] + " because it is not a " +
                    "legal variable, legal number, or open parenthesis");
            }
            //Last token must be a legal variable, legal number, or ')'
            else if (i == tokenList.Count - 1 && !tokenList[i].IsLegalVarNumOrClosedParen())
            {
                throw new FormulaFormatException("The expression cannot end with: " + tokenList[i] + " because it is not a " +
                "legal variable, legal number, or closed parenthesis");
            }
            //The current token is a number
            else if (Double.TryParse(tokenList[i], out double number))
            {
                //The next token must be an operator or a ')'
                if (i != tokenList.Count - 1 && !(tokenList[i + 1].IsArithmeticOperator() || tokenList[i + 1].Equals(")")))
                {
                    throw new FormulaFormatException(tokenList[i + 1] + " cannot follow the number: " + tokenList[i] + " because " +
                    "it is not an operator or a closed parenthesis");
                }
            }
            //The current token is an open parenthesis
            else if (tokenList[i].Equals("("))
            {
                openParenthesisCount++;
                //The next token must be a legal variable, legal number, or '('
                if (i != tokenList.Count - 1 && !tokenList[i + 1].IsLegalVarNumOrOpenParen())
                {
                    throw new FormulaFormatException(tokenList[i + 1] + " cannot follow an open parenthesis because " +
                        "it is not a legal variable, legal number, or closed parenthesis");
                }
            }
            //The current token is an closed parenthesis
            else if (tokenList[i].Equals(")"))
            {
                closedParenthesisCount++;
                //The next token must be an operator or a ')'
                if (i != tokenList.Count - 1 && !(tokenList[i + 1].IsArithmeticOperator() || tokenList[i + 1].Equals(")")))
                {
                    throw new FormulaFormatException(tokenList[i + 1] + " cannot follow a closed parenthesis because " +
                        "it is not an operator or a closed parenthesis");
                }
                //If at any point, there are more closed parenthesis than open parenthesis
                if (closedParenthesisCount > openParenthesisCount)
                {
                    throw new FormulaFormatException("There is one or more excessive closed parenthesis");
                }
            }
            //The current token is one of the four basic arithmetic operators: +, -, *, or /
            //then the next token must be a legal variable, legal number, or '('
            else if (tokenList[i].IsArithmeticOperator())
            {
                if (i != tokenList.Count - 1 && !tokenList[i + 1].IsLegalVarNumOrOpenParen())
                {
                    throw new FormulaFormatException(tokenList[i + 1] + " cannot follow the arithmetic operator: " + tokenList[i] +
                    "because it is not a legal variable, legal number, or closed parenthesis");
                }
            }
            //The current token is a legal variable, but it is not a valid variable
            else if (tokenList[i].IsLegalVariable() && !isValid(normalize(tokenList[i])))
            {
                throw new FormulaFormatException("The legal variable: " + tokenList[i] + " returned an invalid variable" +
                    " according to the given validator");
            }
        }

        if (openParenthesisCount != closedParenthesisCount)
        {
            throw new FormulaFormatException("Each open parenthesis does not have a corresponding closed parenthesis");
        }

        StringBuilder formulaBuilder = new StringBuilder();
        variables = new HashSet<string>();
        foreach (string token in tokenList)
        {
            if (token.IsLegalVariable())
            {
                formulaBuilder.Append(normalize(token));
                variables.Add(normalize(token));
            }
            else if (Double.TryParse(token, out double number))
            {
                formulaBuilder.Append(number);
            }
            else
            {
                formulaBuilder.Append(token);
            }
        }
        formulaString = formulaBuilder.ToString();
    }

    /// <summary>
    /// Evaluates this Formula, using the lookup delegate to determine the values of
    /// variables.  When a variable symbol v needs to be determined, it should be looked up
    /// via lookup(normalize(v)). (Here, normalize is the normalizer that was passed to
    /// the constructor.)
    ///
    /// For example, if L("x") is 2, L("X") is 4, and N is a method that converts all the letters
    /// in a string to upper case:
    ///
    /// new Formula("x+7", N, s => true).Evaluate(L) is 11
    /// new Formula("x+7").Evaluate(L) is 9
    ///
    /// Given a variable symbol as its parameter, lookup returns the variable's value
    /// (if it has one) or throws an ArgumentException (otherwise).
    ///
    /// If no undefined variables or divisions by zero are encountered when evaluating
    /// this Formula, the value is returned.  Otherwise, a FormulaError is returned.
    /// The Reason property of the FormulaError should have a meaningful explanation.
    ///
    /// This method should never throw an exception.
    /// </summary>
    public object Evaluate(Func<string, double> lookup)
    {
        Stack<double> valueStack = new Stack<double>();
        Stack<char> operatorStack = new Stack<char>();
        List<string> tokenList = GetTokens(formulaString).ToList();
        string[] substrings = new string[GetTokens(formulaString).Count()];

        for (int i = 0; i < substrings.Length; i++)
        {
            substrings[i] = tokenList[i];
        }

        foreach (string substring in substrings)
        {
            //If the substring is a mathematical operator
            if (substring.IsMathematicalOperator())
            {
                char oper = char.Parse(substring);

                if ((oper.Equals('+') || oper.Equals('-')) && (operatorStack.IsOnTop('+') || operatorStack.IsOnTop('-')))
                {
                    SimplifyStacks(valueStack, operatorStack);
                }
                else if (oper.Equals(')'))
                {
                    if (operatorStack.IsOnTop('+') || operatorStack.IsOnTop('-'))
                    {
                        SimplifyStacks(valueStack, operatorStack);
                    }
                    if (operatorStack.IsOnTop('('))
                    {
                        operatorStack.Pop();
                    }
                    if (operatorStack.IsOnTop('*'))
                    {
                        SimplifyStacks(valueStack, operatorStack);
                    }
                }
                if (!oper.Equals(')'))
                {
                    operatorStack.Push(oper);
                }
            }
            //If the substring is a double
            else if (Double.TryParse(substring, out double newValue))
            {
                if (operatorStack.IsOnTop('*') || operatorStack.IsOnTop('/'))
                {
                    if (!SimplifyStacks(newValue, valueStack, operatorStack))
                    {
                        return new FormulaError("A division by zero occurred");
                    }
                }
                else
                {
                    valueStack.Push(newValue);
                }
            }
            //If the substring is a variable
            else if (substring.IsLegalVariable() == true)
            {
                double variableValue;
                try
                {
                    variableValue = lookup(substring);
                }
                catch (Exception)
                {
                    return new FormulaError("Unknown variable");
                }
                if (operatorStack.IsOnTop('*') || operatorStack.IsOnTop('/'))
                {
                    if (!SimplifyStacks(variableValue, valueStack, operatorStack))
                    {
                        return new FormulaError("A division by zero occurred");
                    }
                }
                else
                {
                    valueStack.Push(variableValue);
                }
            }
        }
        //All substrings have been processed
        if (operatorStack.Count == 0 && valueStack.Count == 1)
        {
            double finalResult = valueStack.Pop();
            return finalResult;
        }
        else
        {
            if (!SimplifyStacks(valueStack, operatorStack))
            {
                return new FormulaError("A division by zero occurred");
            }
            double finalResult = valueStack.Pop();
            return finalResult;
        }
    }

    /// <summary>
    /// A helper method used to reduce the elements within the stacks by applying appropriate mathematical operations,
    /// thus simplifying the expression. Returns false if there is an occurrence of a division by zero, otherwise it will return true.
    /// </summary>
    /// <param name="newValue"> The most recent double value processed by the Evaluate method </param>
    /// <param name="valueStack"> The stack of floating point numbers that have been processed by the Evaluate method </param>
    /// <param name="operatorStack"> The stack of operators that have been processed by the Evaluate method </param>
    /// <returns> Return false if a division by zero occurred, otherwise it will return true </returns>    
    private static bool SimplifyStacks(double newValue, Stack<double> valueStack, Stack<char> operatorStack)
    {
        if (valueStack.Count > 0)
        {
            double previousValue = valueStack.Pop();
            char oper = operatorStack.Pop();

            if (oper.Equals('*'))
            {
                valueStack.Push(previousValue * newValue);
            }
            else if (oper.Equals('/') && newValue != 0)
            {
                valueStack.Push(previousValue / newValue);
            }
            //A division by zero occurred
            else
            {
                return false;
            }
        }
        //A division by zero did not occur
        return true;
    }

    /// <summary>
    /// A helper method used to reduce the elements within the stacks by applying appropriate mathematical operations,
    /// thus simplifying the expression.
    /// </summary>
    /// <param name="valueStack"> The stack of floating point numbers that have been processed by the Evaluate method </param>
    /// <param name="operatorStack"> The stack of operators that have been processed by the Evaluate method </param>
    private static bool SimplifyStacks(Stack<double> valueStack, Stack<char> operatorStack)
    {
        if (valueStack.Count >= 2)
        {
            double valueOne = valueStack.Pop();
            double valueTwo = valueStack.Pop();
            char oper = operatorStack.Pop();

            if (oper.Equals('+'))
            {
                valueStack.Push(valueTwo + valueOne);
            }
            else if (oper.Equals('-'))
            {
                valueStack.Push(valueTwo - valueOne);
            }
            else if (oper.Equals('*'))
            {
                valueStack.Push(valueTwo * valueOne);
            }
            else if (oper.Equals('/') && valueOne != 0)
            {
                valueStack.Push(valueTwo / valueOne);
            }
            //A division by zero occurred
            else
            {
                return false;
            }
        }
        //A division by zero did not occur
        return true;
    }

    /// <summary>
    /// Enumerates the normalized versions of all of the variables that occur in this
    /// formula.  No normalization may appear more than once in the enumeration, even
    /// if it appears more than once in this Formula.
    ///
    /// For example, if N is a method that converts all the letters in a string to upper case:
    ///
    /// new Formula("x+y*z", N, s => true).GetVariables() should enumerate "X", "Y", and "Z"
    /// new Formula("x+X*z", N, s => true).GetVariables() should enumerate "X" and "Z".
    /// new Formula("x+X*z").GetVariables() should enumerate "x", "X", and "z".
    /// </summary>
    public IEnumerable<string> GetVariables()
    {
        return variables;
    }

    /// <summary>
    /// Returns a string containing no spaces which, if passed to the Formula
    /// constructor, will produce a Formula f such that this.Equals(f).  All of the
    /// variables in the string should be normalized.
    ///
    /// For example, if N is a method that converts all the letters in a string to upper case:
    ///
    /// new Formula("x + y", N, s => true).ToString() should return "X+Y"
    /// new Formula("x + Y").ToString() should return "x+Y"
    /// </summary>
    public override string ToString()
    {
        return this.formulaString;
    }

    /// <summary>
    /// If obj is null or obj is not a Formula, returns false.  Otherwise, reports
    /// whether or not this Formula and obj are equal.
    ///
    /// Two Formulae are considered equal if they consist of the same tokens in the
    /// same order.  To determine token equality, all tokens are compared as strings
    /// except for numeric tokens and variable tokens.
    /// Numeric tokens are considered equal if they are equal after being "normalized" by
    /// using C#'s standard conversion from string to double (and optionally back to a string).
    /// Variable tokens are considered equal if their normalized forms are equal, as
    /// defined by the provided normalizer.
    ///
    /// For example, if N is a method that converts all the letters in a string to upper case:
    ///
    /// new Formula("x1+y2", N, s => true).Equals(new Formula("X1  +  Y2")) is true
    /// new Formula("x1+y2").Equals(new Formula("X1+Y2")) is false
    /// new Formula("x1+y2").Equals(new Formula("y2+x1")) is false
    /// new Formula("2.0 + x7").Equals(new Formula("2.000 + x7")) is true
    /// </summary>
    public override bool Equals(object? obj)
    {
        if (obj == null || !(obj is Formula))
        {
            return false;
        }
        else
        {
            return this.GetHashCode().Equals(obj.GetHashCode());
        }
    }

    /// <summary>
    /// Reports whether f1 == f2, using the notion of equality from the Equals method.
    /// Note that f1 and f2 cannot be null, because their types are non-nullable
    /// </summary>
    public static bool operator ==(Formula f1, Formula f2)
    {
        return f1.Equals(f2);
    }

    /// <summary>
    /// Reports whether f1 != f2, using the notion of equality from the Equals method.
    /// Note that f1 and f2 cannot be null, because their types are non-nullable
    /// </summary>
    public static bool operator !=(Formula f1, Formula f2)
    {
        return !f1.Equals(f2);
    }

    /// <summary>
    /// Returns a hash code for this Formula.  If f1.Equals(f2), then it must be the
    /// case that f1.GetHashCode() == f2.GetHashCode().  Ideally, the probability that two
    /// randomly-generated unequal Formulae have the same hash code should be extremely small.
    /// </summary>
    public override int GetHashCode()
    {
        return formulaString.GetHashCode();
    }

    /// <summary>
    /// Given an expression, enumerates the tokens that compose it.  Tokens are left paren;
    /// right paren; one of the four operator symbols; a legal variable token;
    /// a double literal; and anything that doesn't match one of those patterns.
    /// There are no empty tokens, and no token contains white space.
    /// </summary>
    private static IEnumerable<string> GetTokens(string formula)
    {
        // Patterns for individual tokens
        string lpPattern = @"\(";
        string rpPattern = @"\)";
        string opPattern = @"[\+\-*/]";
        string varPattern = @"[a-zA-Z_](?: [a-zA-Z_]|\d)*";
        string doublePattern = @"(?: \d+\.\d* | \d*\.\d+ | \d+ ) (?: [eE][\+-]?\d+)?";
        string spacePattern = @"\s+";

        // Overall pattern
        string pattern = string.Format("({0}) | ({1}) | ({2}) | ({3}) | ({4}) | ({5})",
                                        lpPattern, rpPattern, opPattern, varPattern, doublePattern, spacePattern);

        // Enumerate matching tokens that don't consist solely of white space.
        foreach (string s in Regex.Split(formula, pattern, RegexOptions.IgnorePatternWhitespace))
        {
            if (!Regex.IsMatch(s, @"^\s*$", RegexOptions.Singleline))
            {
                yield return s;
            }
        }

    }
}

/// <summary>
/// Used to report syntactic errors in the argument to the Formula constructor.
/// </summary>
public class FormulaFormatException : Exception
{
    /// <summary>
    /// Constructs a FormulaFormatException containing the explanatory message.
    /// </summary>
    public FormulaFormatException(string message) : base(message)
    {
    }
}

/// <summary>
/// Used as a possible return value of the Formula.Evaluate method.
/// </summary>
public struct FormulaError
{
    /// <summary>
    /// Constructs a FormulaError containing the explanatory reason.
    /// </summary>
    /// <param name="reason"></param>
    public FormulaError(string reason) : this()
    {
        Reason = reason;
    }

    /// <summary>
    ///  The reason why this FormulaError was created.
    /// </summary>
    public string Reason { get; private set; }
}

/// <summary>
/// This class contains extension methods that are useful in evaluating infix expressions and validating formulas
/// </summary>
public static class FormulaExtensions
{
    /// <summary>
    /// Determines whether this string is a valid mathematical operation for infix expression evaluation.
    /// </summary>
    /// <param name="s"> This string </param>
    /// <returns> True if this string is a valid mathematical operator, false otherwise </returns>
    public static bool IsMathematicalOperator(this string s)
    {
        return s.Equals("+") || s.Equals("-") || s.Equals("*") || s.Equals("/") || s.Equals("(") || s.Equals(")");
    }

    /// <summary>
    /// Determines whether this string is one of the four basic arithmetic operators: +, -, *, or /
    /// This extension is useful for infix expression evaluation.
    /// </summary>
    /// <param name="s"> This string </param>
    /// <returns> True if this string is one of the four basic arithmetic operators, false otherwise </returns>
    public static bool IsArithmeticOperator(this string s)
    {
        return s.Equals("+") || s.Equals("-") || s.Equals("*") || s.Equals("/");
    }
    /// <summary>
    /// Determines whether this string is a legal variable name.
    /// This string is a legal variable name if it consists of a letter or underscore followed by zero or more letters, underscores, or digits.
    /// </summary>
    /// <param name="s"> This string </param>
    /// <returns> True if this string is a legal variable name, false otherwise </returns>
    public static bool IsLegalVariable(this string s)
    {
        string pattern = @"^[a-zA-Z_][a-zA-Z0-9_]*$";
        return Regex.IsMatch(s, pattern);
    }

    /// <summary>
    /// Determines whether this string is a legal number.
    /// A number is legal if it is nonnegative.
    /// </summary>
    /// <param name="s"> This string </param>
    /// <returns> True if this string is a legal number, false otherwise </returns>
    public static bool IsLegalNumber(this string s)
    {
        bool isNumber = Double.TryParse(s, out double number);
        //Return if token is a number and is non-negative
        return isNumber && number >= 0;
    }

    /// <summary>
    /// Determines whether this string is a legal variable name, a legal number, or an open parenthesis.
    /// A variable name is legal if it consists of a letter or underscore followed by zero or more letters, underscores, or digits.
    /// A number is legal if it is nonnegative.
    /// </summary>
    /// <param name="s"> This string </param>
    /// <returns> True if this string is a legal variable name, a legal number, or an open parenthesis, false otherwise </returns>
    public static bool IsLegalVarNumOrOpenParen(this string s)
    {
        return s.IsLegalVariable() || s.IsLegalNumber() || s.Equals("(");
    }

    /// <summary>
    /// Determines whether this string is a legal variable name, a legal number, or an open parenthesis.
    /// A variable name is legal if it consists of a letter or underscore followed by zero or more letters, underscores, or digits.
    /// A number is legal if it is nonnegative.
    /// </summary>
    /// <param name="s"> This string </param>
    /// <returns> True if this string is a legal variable name, a legal number, or an closed parenthesis, false otherwise </returns>
    public static bool IsLegalVarNumOrClosedParen(this string s)
    {
        return s.IsLegalVariable() || s.IsLegalNumber() || s.Equals(")");
    }

    /// <summary>
    /// Determines whether a given character is at the top of this stack.
    /// </summary>
    /// <param name="stack"> This stack of characters </param>
    /// <param name="c"> The given character </param>
    /// <returns> True if the given character is at the top of this stack, false otherwise </returns>
    public static bool IsOnTop(this Stack<char> stack, char c)
    {
        if (stack.Count > 0 && stack.Peek().Equals(c))
        {
            return true;
        }
        else
        {
            return false;
        }
    }
}