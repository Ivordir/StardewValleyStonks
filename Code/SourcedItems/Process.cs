using System;

namespace StardewValleyStonks
{
    public class Process : Selectable, IProcess
    {
        public static Func<Process, Process, int>[] Comparers { get; }

        static Process()
        {
            Comparers = new Func<IProcess, IProcess, int>[4];
            for (int quality = 0; quality < 4; quality++)
            {
                Comparers[quality] = new Func<IProcess, IProcess, int>((p1, p2) => p1.CompareTo(p2, quality));
            }
        }

        public Item Input { get; }
        public virtual int InputAmount => 1;
        public Processor Source { get; }
        public Item OutputItem { get; }
        public override bool Active => base.Active && Source.Active;

        public QualityItem Output(int quality)
        => Source.PreservesQuality
            ? OutputItem.With(quality)
            : OutputItem.Normal;
        public virtual double OutputAmount(int quality) => 1;
        public virtual double Profit(int quality) => Output(quality).Price;
        public virtual double OutputConsume(ref QualityDist inputs)
        {
            double output = inputs;
            inputs -= output;
            return output;
        }
        //public double InputUsed(QualityDist inputs) => OutputFrom(inputs) / OutputAmount(inputs.Quality) * InputAmount;
        public int CompareTo(IProcess other, int quality)
        => Profit(quality).CompareTo(other.Profit(quality));

        public Process(
            Item item,
            Processor sellSource,
            ICondition[] conditions = null)
            : base(true, conditions)
        {
            Input = item;
            Source = sellSource;
            OutputItem = item;
        }
        public Process(
            Item input,
            Processor processor,
            Item output,
            ICondition[] conditions = null)
            : base(true, conditions)
        {
            Input = input;
            Source = processor;
            OutputItem = output;
        }
    }
}
