namespace StardewValleyStonks
{
    public interface ICondition
    {
        public bool Override { get; set; }
        public bool IsMet { get; }
        public string WarningMessage { get; }
    }
}
