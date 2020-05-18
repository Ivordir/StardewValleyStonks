using System.Collections.Generic;

namespace StardewValleyStonks
{
    public class SellItem : Selectable, IProcess
    {
        public Source Source { get; }
        public Dictionary<IItem, int> Inputs { get; }
        public IItem OutputItem { get; }
        public double OutputAmount => 1;

        public SellItem(
            IItem item,
            Source source,
            ICondition[] conditions = null)
            : base(true, conditions)
        {
            Source = source;
            Inputs = new Dictionary<IItem, int> { { item, 1 } };
            OutputItem = item;
        }

        public double MaxOutput(Dictionary<IItem, double> inputs)
        {
            return inputs[OutputItem];
        }

        public double Profit(Dictionary<IItem, double> inputs)
        {
            double maxOutput = MaxOutput(inputs);
            inputs.Remove(OutputItem);
            return maxOutput * OutputItem.Price;
        }
    }
}
