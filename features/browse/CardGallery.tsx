import Grid from '@material-ui/core/Grid';
import Typography from '@material-ui/core/Typography';
import { Dispatch, SetStateAction, useState } from 'react';
import styled from 'styled-components';
import { useAuthentication } from '../../auth/AuthenticationProvider';
import { PriceTypes } from './browseSlice';
import CardBox from './CardBox';
import GalleryControls from './GalleryControls';
import { Card } from './types/Card';

interface CardGalleryProps {
  cards: Card[];
  cardsPerRow?: number;
  page: number;
  first: number;
  skip: number;
  setSkip: Dispatch<SetStateAction<number>>;
  setFirst: Dispatch<SetStateAction<number>>;
  setPage: Dispatch<SetStateAction<number>>;
  totalResults: number;
  priceType: PriceTypes;
  collectionByCardId?: [
    {
      cardID: string;
      quantityReg: number;
      quantityFoil: number;
    }
  ];
  userId?: string;
  isLoading?: boolean;
  isFetching?: boolean;
}

const CardGallery: React.FC<CardGalleryProps> = ({
  cards,
  first,
  skip,
  page,
  totalResults,
  setSkip,
  setFirst,
  setPage,
  priceType,
  collectionByCardId,
  userId,
  isLoading,
  isFetching,
}) => {
  const [cardsPerRow, setCardsPerRow] = useState(4);
  const [galleryWidth, setGalleryWidth] = useState(100);
  const [nameIsVisible, setNameIsVisible] = useState(true);
  const [setIsVisible, setSetIsVisible] = useState(true);
  const [priceIsVisible, setPriceIsVisible] = useState(true);

  const { user } = useAuthentication();

  const atLeastOneCardToShow = totalResults > 0;

  const settingGroups = [
    {
      label: 'Show Fields',
      type: 'toggleFilters',
      settings: [
        {
          key: 'name',
          label: 'Name',
          isVisible: nameIsVisible,
          setVisibility: setNameIsVisible,
        },
        {
          key: 'set',
          label: 'Set',
          isVisible: setIsVisible,
          setVisibility: setSetIsVisible,
        },
        {
          key: 'price',
          label: 'Price',
          isVisible: priceIsVisible,
          setVisibility: setPriceIsVisible,
        },
      ],
    },
  ];

  if (isLoading) {
    return (
      <>
        <GalleryControls
          items={cards}
          first={first}
          page={page}
          setCardsPerRow={setCardsPerRow}
          setFirst={setFirst}
          setGalleryWidth={setGalleryWidth}
          setPage={setPage}
          setSkip={setSkip}
          skip={skip}
          totalResults={totalResults}
          cardsPerRow={cardsPerRow}
          settingGroups={settingGroups}
          galleryType="cards"
          isLoading={isLoading}
          isFetching={isFetching}
        />
      </>
    );
  }

  if (atLeastOneCardToShow) {
    return (
      <>
        <GalleryControls
          items={cards}
          first={first}
          page={page}
          setCardsPerRow={setCardsPerRow}
          setFirst={setFirst}
          setGalleryWidth={setGalleryWidth}
          setPage={setPage}
          setSkip={setSkip}
          skip={skip}
          totalResults={totalResults}
          cardsPerRow={cardsPerRow}
          settingGroups={settingGroups}
          galleryType="cards"
        />

        <CardGalleryWrapper cardsPerRow={cardsPerRow} galleryWidth={galleryWidth}>
          {cards &&
            cards.map((card) => (
              <CardBox
                key={card.id}
                card={card}
                priceType={priceType}
                nameIsVisible={nameIsVisible}
                setIsVisible={setIsVisible}
                priceIsVisible={priceIsVisible}
                userId={userId}
                loggedInUserId={user?.id}
                quantityReg={collectionByCardId?.[card.id]?.quantityReg ?? 0}
                quantityFoil={collectionByCardId?.[card.id]?.quantityFoil ?? 0}
              />
            ))}
        </CardGalleryWrapper>
      </>
    );
  }
  if (!isLoading && !isFetching && totalResults === 0 && cards?.length === 0)
    return (
      <Grid container alignItems="center" justify="center">
        <Grid item>
          <Typography variant="h6">No results found -- try another search!</Typography>
        </Grid>
      </Grid>
    );

  return null;
};

interface CardGalleryWrapperProps {
  cardsPerRow: number;
  galleryWidth: number;
}

const CardGalleryWrapper = styled.div<CardGalleryWrapperProps>(({ cardsPerRow = 4, galleryWidth = 100 }) => ({
  display: 'grid',
  gridTemplateColumns: `repeat(${cardsPerRow}, minmax(0, 1fr))`,
  gridTemplateRows: 'repeat(3, 1fr)',
  gap: `5px 10px`,
  width: `${galleryWidth}%`,
  margin: '0 auto',
}));

export default CardGallery;
