import styled from 'styled-components';
import LazyLoad from 'react-lazyload';

interface CardBoxProps {
  card: Card;
}

const CardBox: React.FC<CardBoxProps> = ({ card }) => {
  const imageUrl = `https://mtgcb-images.s3.amazonaws.com/cards/images/normal/${card.id}.jpg`;
  return (
    <CardWrapper key={card.id}>
      <CardAttributes>
        <LazyLoad key={`lazy-${card.id}`} once resize height={50}>
          <CardImage alt={card.name} title={card.name} src={imageUrl} />
        </LazyLoad>
        <CardName title={card.name}>{card.name}</CardName>
        <CardSet title={card.set.name}>{card.set.name}</CardSet>
      </CardAttributes>
    </CardWrapper>
  );
};

export interface Card {
  id: number;
  name: string;
  set: {
    name: string;
  };
}

const CardWrapper = styled.div({
  display: 'inline-block',
  fontSize: 'clamp(12px, 1.0vw, 22px)',
  height: 'auto',
  minHeight: '50px',
});

const CardAttributes = styled.div({ textAlign: 'center', maxWidth: '100%' });

const CardName = styled.div({ whiteSpace: 'nowrap', overflow: 'hidden', textOverflow: 'ellipsis', maxWidth: '100%' });

const CardSet = styled.em(({ theme }) => ({
  display: 'block',
  whiteSpace: 'nowrap',
  overflow: 'hidden',
  textOverflow: 'ellipsis',
  maxWidth: '100%',
  color: theme.palette.text.secondary,
}));

const CardImage = styled.img({ width: '100%', height: 'auto', borderRadius: '5%' });

export default CardBox;
