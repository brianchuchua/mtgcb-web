export interface Card {
  id: number;
  name: string;
  set: {
    id: string;
    parentSetId?: {
      id: string;
    };
    name: string;
    slug: string;
    code?: string;
    category?: string;
    setType?: string;
    cardCount?: number;
    releasedAt?: string;
    sealedProductUrl?: string;
    isDraftable?: boolean;
  };
  low: number;
  average: number;
  high: number;
  market: number;
  foil: number;
  tcgplayerId: number;
  priceId?: {
    low: number;
    average: number;
    high: number;
    market: number;
    foil: number;
  };
}
