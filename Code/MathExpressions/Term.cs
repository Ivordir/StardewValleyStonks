namespace StardewValleyStonks
{
    public readonly struct Term : IValue
    {
        public double Value
        {
            get
            {
                double value = 1;
                foreach (IValue amount in Amounts)
                {
                    value *= amount.Value;
                }
                return Negative ? -1 * value : value;
            }
        }

        readonly IValue[] Amounts;
        readonly bool Negative;

        public Term(IValue coefficient, IValue variable, bool negative = false)
        {
            Amounts = new IValue[] { coefficient, variable };
            Negative = negative;
        }
        public Term(IValue[] amounts, bool negative = false)
        {
            Amounts = amounts;
            Negative = negative;
        }
    }
}
