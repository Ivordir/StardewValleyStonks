namespace StardewValleyStonks
{
    public struct Term : IValue
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
                return Negative ? -1 * value : value;
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
