using System;
using NUnit.Framework;

namespace CalculatorTest
{
    [TestFixture]
    public class CalculatorTest
    {
        [TestCase("11+1", "CalcInt:12")]
        [TestCase("1+1.0", "CalcReal:2")]
        [TestCase("1 + .7", "CalcReal:1.7")]
        [TestCase("1 + 3.", "CalcReal:4")]
        [TestCase("1.123+1.0", "CalcReal:2.123")]
        [TestCase("2147483647+1", "Err.OverFlow")]

        [TestCase("1-21", "CalcInt:-20")]
        [TestCase("1.0 - 1", "CalcReal:0")]
        [TestCase("1.0 - 1.123", "CalcReal:-0.123")]
        [TestCase("-2147483647-2", "Err.OverFlow")]

        [TestCase("11*1", "CalcInt:11")]
        [TestCase("11*0.1", "CalcReal:1.1")]
        [TestCase("2147483647*3", "Err.OverFlow")]
        [TestCase("2147483647*1", "CalcInt:2147483647")]

        [TestCase("10/2", "CalcInt:5")]
        [TestCase("11/2", "CalcReal:5.5")]
        [TestCase("10/0", "Err.ZeroDiv")]

        //[TestCase("1+sin(90)", "CalcReal:2")]
        //[TestCase("1+sin(180)", "CalcReal:1")]

        //[TestCase("1+PI", "CalcReal:4.141593")]

        [TestCase("\"abc\" + \"de f\"", "CalcString:abcde f")]
        [TestCase("\"abc\" * 3", "CalcString:abcabcabc")]
        [TestCase("4*\"abc\"", "CalcString:abcabcabcabc")]
        //[TestCase("5 + LEN(\"abc\")", "CalcInt:8")]

        //[TestCase("5+\"abc\"", "Err.UnSupportCalcRule")]
        //[TestCase("\"abc\"-1.2", "Err.UnSupportCalcRule")]
        //[TestCase("\"abcde\"- \"bcd\"  ", "CalcString:ae")]
        [TestCase("\"abcde\"- \"efg\"", "Err.CantMinusString")]

        [TestCase("\"漢字\" ( 2 + 1 )", "CalcString:漢字漢字漢字")]
        public void Test(string input, string expected)
        {
            Assert.AreEqual(expected, Calculator.Calculator.Execute(input));
        }
    }
}
