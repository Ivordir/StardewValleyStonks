namespace StardewValleyStonks
{
    public interface IProcessor : ISource
    {
        public bool MutableQuality { get; }
        public bool PreservesQuality { get; set; }
    }
}
