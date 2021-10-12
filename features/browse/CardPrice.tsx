import styled from 'styled-components';
import { PriceTypes } from './browseSlice';
import { Card, formatPrice } from './util/formatPrice';

interface CardPriceProps {
  card: Card;
  priceType: PriceTypes;
}

const CardPrice: React.FC<CardPriceProps> = ({ card, priceType }) => (
  <CardPriceWrapper key={`${card.id}-price-${priceType}`}>
    {formatPrice(card, priceType)}
    {formatPrice(card, 'foil')}
  </CardPriceWrapper>
);

const CardPriceWrapper = styled.div(({ theme }) => ({
  color: theme.palette.text.secondary,
}));

export default CardPrice;
