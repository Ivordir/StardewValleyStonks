namespace StardewValleyStonks
{
    public interface IPriceMultiplier : IMultiplier
    {
        public int ApplyTo(int basePrice);
    }
}