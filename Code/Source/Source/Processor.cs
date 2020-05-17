namespace StardewValleyStonks
{
    public class Processor : Source, IProcessor
    {
        private readonly static Product[] None = new Product[0];

        public bool MutableQuality => Products != None;
        public bool PreservesQuality
        {
            get => PreservesQuality;
            set
            {
                PreservesQuality = value;
                if (MutableQuality)
                {
                    foreach (Product product in Products)
                    {
                        product.Qualities = value;
                    }
                }
            }
        }

        private readonly Product[] Products;

        public Processor(
            string name,
            bool preservesQuality = true,
            Product[] products = null,
            bool enabled = true,
            ICondition[] conditions = null)
            : base(name, enabled, conditions)
        {
            PreservesQuality = preservesQuality;
            Products = products ?? None;
        }
    }
}
