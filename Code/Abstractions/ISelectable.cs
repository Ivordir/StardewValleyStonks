namespace StardewValleyStonks
{
    public interface ISelectable
    {
        public bool Selected { get; set; }
        public bool Active { get; }
    }
}