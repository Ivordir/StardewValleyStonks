namespace StardewValleyStonks
{
    public class ExclusiveCondition : ICondition
    {
        public IActiveItem ExclusiveWith { get; }

        public ExclusiveCondition(IActiveItem exclusiveWith)
        {
            ExclusiveWith = exclusiveWith;
        }

        public bool IsMet => !ExclusiveWith.Enabled;

        public string WarningMessage => $"{ExclusiveWith} cannot be enabled.";
    }
}
