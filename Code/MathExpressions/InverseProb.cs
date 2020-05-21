namespace StardewValleyStonks
{
    public class InverseProb : IValue
    {
        public double Value => 1 - Prob.Value;

        private readonly IValue Prob;

        public InverseProb(IValue prob)
        {
            Prob = prob;
        }
    }
}
