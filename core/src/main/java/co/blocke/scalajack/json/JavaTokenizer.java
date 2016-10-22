package co.blocke.scalajack.json;

import co.blocke.scalajack.TokenType;

public class JavaTokenizer {

    private final static scala.Enumeration.Value BeginObjectTokenType = TokenType.BeginObject();
    private final static scala.Enumeration.Value EndObjectTokenType = TokenType.EndObject();
    private final static scala.Enumeration.Value BeginArrayTokenType = TokenType.BeginArray();
    private final static scala.Enumeration.Value EndArrayTokenType = TokenType.EndArray();
    private final static scala.Enumeration.Value StringTokenType = TokenType.String();
    private final static scala.Enumeration.Value NumberTokenType = TokenType.Number();

    public TokenReader tokenize(char[] source, int offset, int length) {
        int position = offset;
        int maxPosition = offset + length;

        int maxNumberOfTokens = 1024;
        int numberOfTokens = 0;

        scala.Enumeration.Value[] tokenTypes = new scala.Enumeration.Value[maxNumberOfTokens];
        int[] tokenOffsets = new int[maxNumberOfTokens];
        int[] tokenLengths = new int[maxNumberOfTokens];

        int lastPosition = -1;

        while (position < maxPosition) {
//                System.out.println(position + " (" + source[position] + ")");

            if (position == lastPosition) {
                throw new RuntimeException("Position did not increment beyond " + lastPosition);
            }

            lastPosition = position;

            int initialPosition = position;
            char initialChar = source[position];
            switch (initialChar) {
                case '{': {
                    int i = numberOfTokens;
                    numberOfTokens += 1;

                    tokenTypes[i] = BeginObjectTokenType;
                    tokenOffsets[i] = position;
                    tokenLengths[i] = 1;

                    position += 1;
                    break;
                }

                case '}': {
                    int i = numberOfTokens;
                    numberOfTokens += 1;

                    tokenTypes[i] = EndObjectTokenType;
                    tokenOffsets[i] = position;
                    tokenLengths[i] = 1;

                    position += 1;
                    break;
                }

                case '[': {
                    int i = numberOfTokens;
                    numberOfTokens += 1;

                    tokenTypes[i] = BeginArrayTokenType;
                    tokenOffsets[i] = position;
                    tokenLengths[i] = 1;

                    position += 1;
                    break;
                }

                case ']': {
                    int i = numberOfTokens;
                    numberOfTokens += 1;

                    tokenTypes[i] = EndArrayTokenType;
                    tokenOffsets[i] = position;
                    tokenLengths[i] = 1;

                    position += 1;
                    break;
                }

                case ':': {
                    position += 1;
                    break;
                }

                case ',': {
                    position += 1;
                    break;
                }

                case ' ': {
                    position += 1;
                    break;
                }

                case '\r': {
                    position += 1;
                    break;
                }

                case '\n': {
                    position += 1;
                    break;
                }

                case '\t': {
                    position += 1;
                    break;
                }

                case '"': {
                    int startOfString = position;

                    position += 1; // Skip the leading double-quote

                    while (position < maxPosition && source[position] != '"') {
                        if (source[position] == '\\') {
                            position += 2;
                        } else {
                            position += 1;
                        }
                    }

                    position += 1; // Skip the trailing double-quote

                    int i = numberOfTokens;
                    numberOfTokens += 1;

                    tokenTypes[i] = StringTokenType;
                    tokenOffsets[i] = startOfString;
                    tokenLengths[i] = position - startOfString;

                    break;
                }

                case '0':
                case '1':
                case '2':
                case '3':
                case '4':
                case '5':
                case '6':
                case '7':
                case '8':
                case '9': {
                    int startOfNumber = position;
                    position += 1; // Skip the first digit

                    boolean buffering = true;

                    while (position < maxPosition && buffering) {
                        switch (source[position]) {
                            case '0':
                            case '1':
                            case '2':
                            case '3':
                            case '4':
                            case '5':
                            case '6':
                            case '7':
                            case '8':
                            case '9': {
                                position += 1;
                                break;
                            }

                            default:
                                buffering = false;
                                break;
                        }
                    }

                    int i = numberOfTokens;
                    numberOfTokens += 1;

                    tokenTypes[i] = NumberTokenType;
                    tokenOffsets[i] = startOfNumber;
                    tokenLengths[i] = position - startOfNumber;

                    break;
                }

                default:
                    System.out.println("Unknown character at position " + initialPosition + ": " + initialChar);
                    throw new RuntimeException("Unknown character at position " + initialPosition + ": " + initialChar);
            }
        }

//            System.out.println("DONE");

        return new TokenReader(source, numberOfTokens, tokenTypes, tokenOffsets, tokenLengths);
    }

}
