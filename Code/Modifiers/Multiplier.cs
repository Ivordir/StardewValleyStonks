namespace StardewValleyStonks
{
    public class Multiplier : Selectable, IMultiplier
    {
        public string Name { get; }
        public double Value { get; }

        public Multiplier(
            string name,
            double value,
            bool selected = true,
            ICondition[] conditions = null)
            : base(selected, conditions)
        {
            Name = name;
            Value = value;
        }
    }
}
