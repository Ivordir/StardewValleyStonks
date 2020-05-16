namespace StardewValleyStonks
{
    public interface IInput
    {
        public IPriceTracker<ISource, IPrice> InputItem { get; }
        public double OutputPerInput { get; }

        public double UnitPrice { get; }
        public double Output { get; }
    }
}