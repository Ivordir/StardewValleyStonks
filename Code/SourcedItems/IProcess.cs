namespace StardewValleyStonks
{
    public interface IProcess : ISelectable
    {
        public Item Input { get; }
        public int InputAmount { get; }
        public Processor Source { get; }

        public QualityItem Output(int quality);
        public double OutputAmount(int quality);
        public double Profit(int quality);
        public double OutputConsume(ref QualityDist inputs);
        public double OutputConsume(ref QualityDist inputs, double maxOutput);
        public int CompareTo(IProcess other, int quality);
    }
}
