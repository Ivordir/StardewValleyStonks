namespace StardewValleyStonks
{
    public class Exponent : IValue
    {
        public double Value => System.Math.Pow(Base.Value, Power.Value);

        private readonly IValue Base, Power;

        public Exponent(IValue baseValue, IValue power)
        {
            Base = baseValue;
            Power = power;
        }
    }
}
