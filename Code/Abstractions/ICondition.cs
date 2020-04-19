namespace StardewValleyStonks
{
    public interface ICondition
    {
        public bool IsMet { get; }
        public string WarningMessage { get; }
    }
}
