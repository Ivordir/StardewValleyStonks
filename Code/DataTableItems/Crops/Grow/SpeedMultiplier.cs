namespace StardewValleyStonks
{
    public class SpeedMultiplier : Selectable, IMultiplier
    {
        public SpeedMultiplier(
            string name,
            double value,
            bool selected = false,
            ICondition[] conditions = null)
            : base(selected, conditions)
        {
            Name = name;
            Value = value;
        }
        public string Name { get; }
        public double Value { get; }
    }
}
