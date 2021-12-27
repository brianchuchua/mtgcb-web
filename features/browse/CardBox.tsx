import LazyLoad from 'react-lazyload';
import styled from 'styled-components';
import Link from '../../components/Link';
import { PriceTypes } from './browseSlice';
import CardPrice from './CardPrice';
import CardQuantity from './CardQuantity';

interface CardBoxProps {
  card: Card;
  priceType: PriceTypes;
  nameIsVisible?: boolean;
  priceIsVisible?: boolean;
  setIsVisible?: boolean;
  userId?: string;
  quantityReg?: number;
  quantityFoil?: number;
}

const CardBox: React.FC<CardBoxProps> = ({
  card,
  priceType,
  nameIsVisible = true,
  priceIsVisible = true,
  setIsVisible = true,
  userId = null,
  quantityReg = null,
  quantityFoil = null,
}) => {
  const imageUrl = `https://mtgcb-images.s3.amazonaws.com/cards/images/normal/${card.id}.jpg`;
  return (
    <CardWrapper key={card.id}>
      <CardAttributes>
        <LazyLoad key={`lazy-${card.id}`} once resize height={50}>
          <a href={generateCardUrl(card.tcgplayerId, card.name)} target="_blank" rel="noreferrer">
            <CardImage alt={card.name} title={card.name} src={imageUrl} set={card.set?.name} />
          </a>
        </LazyLoad>
        {nameIsVisible && <CardName title={card.name}>{card.name}</CardName>}
        {setIsVisible && (
          <Link href={userId ? `/collections/${userId}/sets/${card.set?.slug}` : `/browse/sets/${card.set?.slug}`}>
            <CardSet title={card.set?.name}>{card.set?.name ?? 'Unknown Set'}</CardSet>
          </Link>
        )}
        {userId && <CardQuantity quantityReg={quantityReg} quantityFoil={quantityFoil} />}
        {priceIsVisible && <CardPrice card={card} priceType={priceType} />}
      </CardAttributes>
    </CardWrapper>
  );
};

const generateCardUrl = (cardId: string | number, cardName) =>
  cardId
    ? `https://shop.tcgplayer.com/magic/product/productsearch?id=${cardId}&utm_campaign=affiliate&utm_medium=CTNBLDR&utm_source=CTNBLDR`
    : `https://www.tcgplayer.com/search/magic/product?productLineName=magic&q=${cardName}&utm_campaign=affiliate&utm_medium=CTNBLDR&utm_source=CTNBLDR`;

export interface Card {
  id: number;
  name: string;
  set: {
    name: string;
    slug: string;
  };
  low: number;
  average: number;
  high: number;
  market: number;
  foil: number;
  tcgplayerId: number;
}

const CardWrapper = styled.div({
  display: 'inline-block',
  fontSize: 'clamp(12px, 1.0vw, 22px)',
  height: 'auto',
  minHeight: '50px',
  lineHeight: 1.43,
});

const CardAttributes = styled.div({ textAlign: 'center', maxWidth: '100%' });

const CardName = styled.div({ whiteSpace: 'nowrap', overflow: 'hidden', textOverflow: 'ellipsis', maxWidth: '100%' });

const CardSet = styled.em(({ theme }) => ({
  display: 'block',
  whiteSpace: 'nowrap',
  overflow: 'hidden',
  textOverflow: 'ellipsis',
  maxWidth: '100%',
}));

interface CardImageProps {
  set?: string;
}

const CardImage = styled.img<CardImageProps>(({ set }) => ({
  width: '100%',
  height: 'auto',
  borderRadius: set === 'Limited Edition Alpha' ? '7%' : '5%',
}));

export default CardBox;
