namespace StardewValleyStonks
{
    public class DependentCondition : ICondition
    {
        public IActiveItem DependentOn { get; }

        public DependentCondition(IActiveItem dependentOn)
        {
            DependentOn = dependentOn;
        }

        public bool IsMet => DependentOn.Active;
        public string WarningMessage => $"{DependentOn} must be active.";
    }
}
