namespace StardewValleyStonks
{
    public class ProductSource : Source, IProductSource
    {
        public bool HasQuality { get; set; }

        public ProductSource(string name, bool enabled = true, ICondition[] conditions = null, bool quality = true) : base(name, enabled, conditions)
        {
            HasQuality = quality;
        }
    }
}
