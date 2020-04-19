using Microsoft.AspNetCore.Components;
using System;

namespace StardewValleyStonks
{
    public class Fertilizer : DataTableItem
    {
        public int Quality { get; }
        public float Speed { get; }
        //public IPriceManager<Source, IPricedItem> PriceManager { get; }
        //private Fertilizer Superior; //tree where the parent node is a superior fertilizer
        //private List<Fertilizer> Inferiors; //immediate children

        public Fertilizer(string name, int quality, float speed, IManager<ISource, IPricedItem> priceManager) : base(name, priceManager)
        {
            Quality = quality;
            Speed = speed;
            //FindSuperior();
        }

        /*
        public Fertilizer SuperiorFert
        {
            get
            {
                if (Superior == null)
                {
                    return null;
                }
                else if (Superior.Superior == null)
                {
                    return Superior;
                }
                return Superior.SuperiorFert;
            }
        }
        */
        public override bool Active { get => throw new NotImplementedException(); }

        public int Price => PriceManager.Price();

        /*
        public void FindSuperior()
        {
            Fertilizers.Remove(this);
            for (int i = 0; i < Fertilizers.Count; i++)
            {
                Fertilizer fert = Fertilizers[i];
                if (InferiorTo(fert))
                {
                    fert.Inferiors.Add(this);
                    Superior = fert;
                }
            }
            Superior = null;
            Fertilizers.Add(this);
        }

        public bool InferiorTo(Fertilizer fert)
        {
            return fert.Price <= Price && fert.Speed >= Speed && fert.Quality >= Quality;
        }

        private new void SourceWasEnabled(ProductSource source)
        {
            if (Source == null || PriceFrom[Source] < Price)
            {
                Source = source;
                UpdateTree();
            }
        }

        private void FindBestSource()
        {
            int cheapestPrice = int.MaxValue;
            bool oneValidSource = false;
            foreach (KeyValuePair<ProductSource, int> pair in PriceFrom)
            {
                if (SourceEnabled[pair.Key] && pair.Value < cheapestPrice)
                {
                    oneValidSource = true;
                    cheapestPrice = pair.Value;
                    Source = pair.Key;
                }
            }
            if (oneValidSource)
            {
                UpdateTree();
            }
            else
            {
                Source = null;
                for (int i = 0; i < Inferiors.Count; i++)
                {
                    Fertilizer fert = Inferiors[i];
                    fert.Superior = Superior;
                }
                Inferiors.Clear();
                Superior = null;
            }
        }

        private void UpdateTree()
        {
            Fertilizer oldSuperior = Superior;
            FindSuperior();
            if (Superior != null && olderSuperior != Superior && )
            {

            }
        }
        */
    }
}
