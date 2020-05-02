namespace StardewValleyStonks.Code.Modifiers
{
    public class SpeedMultiplier : Selectable, ISpeedMultiplier
    {
        public SpeedMultiplier(
            double value,
            bool selected = false,
            ICondition[] conditions = null)
            : base(selected, conditions)
        {
            Value = value;
        }

        public bool HasTableColumn => true;
        public double Value { get; }
    }
}
