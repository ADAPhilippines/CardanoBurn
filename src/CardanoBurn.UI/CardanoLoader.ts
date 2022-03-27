class CardanoLoader {
  public static Cardano: typeof import("@emurgo/cardano-serialization-lib-browser/cardano_serialization_lib") | null = null;

  public static async LoadAsync() {
      if (this.Cardano === null) {
          this.Cardano = await import( "@emurgo/cardano-serialization-lib-browser/cardano_serialization_lib");
      }
      return this.Cardano;
  }
}

export default CardanoLoader;