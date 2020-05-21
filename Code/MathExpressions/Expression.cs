namespace StardewValleyStonks
{
    public class Expression : IValue
    {
        public double Value
        {
            get
            {
                double sum = 0;
                foreach(IValue term in Terms)
                {
                    sum += term.Value;
                }
                return sum;
            }
        }

        private readonly IValue[] Terms;

        public Expression(IValue[] terms)
        {
            Terms = terms;
        }
    }
}
