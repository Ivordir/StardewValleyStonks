namespace StardewValleyStonks
{
    public class Reference<T>
    {
        public T Ref { get; set; }

        public Reference(T value) => Ref = value;
    }
}
