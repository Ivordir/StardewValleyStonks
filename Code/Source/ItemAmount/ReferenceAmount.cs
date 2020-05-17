namespace StardewValleyStonks
{
    public class ReferenceAmount : IItemAmount
    {
        public ReferenceAmount(IItem item, Reference<double> refAmount)
        {
            Item = item;
            Ref = refAmount;
        }

        public IItem Item { get; }
        public double Amount => Ref.Value;
        private readonly Reference<double> Ref;
    }
}
