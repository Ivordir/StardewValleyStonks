namespace StardewValleyStonks
{
    public interface ICondition
    {
        public bool IsMet { get; }
        public bool Override { get; set; }
        public string WarningMessage { get; }
    }
}
