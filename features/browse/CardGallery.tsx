import Grid from '@material-ui/core/Grid';
import Typography from '@material-ui/core/Typography';
import { Dispatch, SetStateAction } from 'react';
import styled, { keyframes } from 'styled-components';
import { useAuthentication } from '../../auth/AuthenticationProvider';
import Wubrg from '../../components/loaders/Wubrg';
import { useLocalStorage } from '../../util';
import { PriceTypes } from './browseSlice';
import CardBox from './CardBox';
import GalleryControls from './GalleryControls';
import { Card } from './types/Card';

interface CardGalleryProps {
  cards: Card[];
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
  isFetching: boolean;
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
  const isSetPage = window?.location?.href?.includes('/sets/');
  const [cardsPerRow, setCardsPerRow] = useLocalStorage('cardsPerRow', 5);
  const [galleryWidth, setGalleryWidth] = useLocalStorage('cardSize', 100);
  const [nameIsVisible, setNameIsVisible] = useLocalStorage('cardNameIsVisible', true);
  const [setIsVisible, setSetIsVisible] = useLocalStorage('cardSetIsVisible', !isSetPage);
  const [priceIsVisible, setPriceIsVisible] = useLocalStorage('cardPriceIsVisible', true);

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

  if (isLoading || (isFetching && userId)) {
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
          galleryWidth={galleryWidth}
          settingGroups={settingGroups}
          galleryType="cards"
          isLoading={isLoading}
          isFetching={isFetching}
        />
        <FadeIn style={{ marginTop: '100px' }}>
          <Wubrg />
        </FadeIn>
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
          galleryWidth={galleryWidth}
          settingGroups={settingGroups}
          isFetching={isFetching}
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
                quantityReg={collectionByCardId?.[card.id]?.quantityReg ?? null}
                quantityFoil={collectionByCardId?.[card.id]?.quantityFoil ?? null}
              />
            ))}
        </CardGalleryWrapper>
        <div style={{ marginTop: '20px' }}>
          <GalleryControls
            items={cards}
            first={first}
            page={page}
            galleryWidth={galleryWidth}
            setCardsPerRow={setCardsPerRow}
            setFirst={setFirst}
            setGalleryWidth={setGalleryWidth}
            setPage={setPage}
            setSkip={setSkip}
            skip={skip}
            totalResults={totalResults}
            cardsPerRow={cardsPerRow}
            settingGroups={settingGroups}
            isFetching={isFetching}
            galleryType="cards"
            isOnBottom
          />
        </div>
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

const CardGalleryWrapper = styled.div<CardGalleryWrapperProps>(({ cardsPerRow = 5, galleryWidth = 100 }) => ({
  display: 'grid',
  gridTemplateColumns: `repeat(${cardsPerRow}, minmax(0, 1fr))`,
  gridTemplateRows: 'repeat(1, 1fr)',
  gap: `5px 10px`,
  width: `${galleryWidth}%`,
  margin: '0 auto',
}));

const fadeIn = keyframes`
    0% { opacity: 0; }
    66% { opacity: 0; }
    100% { opacity: 1; }
`;

const FadeIn = styled.div`
  animation: ${fadeIn} 1.5s ease-in;
`;

export default CardGallery;
