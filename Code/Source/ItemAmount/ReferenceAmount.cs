namespace StardewValleyStonks
{
    public class ReferenceAmount : IItemAmount
    {
        public ReferenceAmount(IProduct item, Reference<double> refAmount)
        {
            Item = item;
            Ref = refAmount;
        }

        public IProduct Item { get; }
        public double Amount => Ref.Value;
        private readonly Reference<double> Ref;
    }
}
