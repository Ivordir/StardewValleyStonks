namespace StardewValleyStonks
{
    public class Term : IValue
    {
        public double Value
        {
            get
            {
                double value = 1;
                foreach(IValue amount in Amounts)
                {
                    value *= amount.Value;
                }
                return Negative ? value * -1 : value;
            }
        }

        private readonly IValue[] Amounts;
        private readonly bool Negative;

        public Term(IValue[] amounts, bool negative = false)
        {
            Amounts = amounts;
            Negative = negative;
        }
    }
}
