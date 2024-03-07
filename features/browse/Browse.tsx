import { memo } from 'react';
import { useSelector } from 'react-redux';
import styled from 'styled-components';
import Breadcrumbs from '../../components/layout/Breadcrumbs';
import { ResponsiveContainer } from '../../components/layout/ResponsiveContainer';
import { useGetCostToPurchaseAllQuery } from '../../network/services/mtgcbApi';
import { RootState } from '../../redux/rootReducer';
import { setFormVisibility } from './browseSlice';
import CardGallery from './CardGallery';
import CardTable from './CardTable';
import { useCardSearch } from './hooks/useCardSearch';
import { useExpansionSearch } from './hooks/useExpansionSearch';
import SetGallery from './SetGallery';
import SetTable from './SetTable';

export const Browse: React.FC = () => {
  const reduxSlice = 'browse';
  const { viewSubject, viewMode, priceType } = useSelector((state: RootState) => state[reduxSlice]);

  const {
    cards,
    isCardDataLoading,
    isCardDataFetching,
    isCardMetaDataLoading,
    isCardMetaDataFetching,
    skip,
    setSkip,
    page,
    setPage,
    first,
    setFirst,
    totalResults,
  } = useCardSearch(reduxSlice, setFormVisibility);

  const {
    expansions,
    isGetAllSetsLoading,
    isGetAllSetsFetching,
    isAllSetsMetaLoading,
    isAllSetsMetaFetching,
    expansionsSkip,
    setExpansionsSkip,
    expansionsFirst,
    setExpansionsFirst,
    expansionsPage,
    setExpansionsPage,
    totalExpansionsResults,
  } = useExpansionSearch();

  const {
    data: costToPurchaseAll,
    isLoading: isCostsToPurchaseLoading,
    isFetching: isCostsToPurchaseFetching,
  } = useGetCostToPurchaseAllQuery();
  const costsToPurchase = costToPurchaseAll?.data?.costToPurchaseAll?.costToPurchaseAll;

  const isLoading = isCardDataLoading || isCardMetaDataLoading || isCostsToPurchaseLoading || isGetAllSetsLoading || isAllSetsMetaLoading;
  const isFetching =
    isCardDataFetching || isCardMetaDataFetching || isCostsToPurchaseFetching || isGetAllSetsFetching || isAllSetsMetaFetching;

  return (
    <ResponsiveContainer maxWidth="xl">
      <ContentWrapper>
        <Breadcrumbs
          links={[
            {
              title: 'Browse',
              url: '/browse',
            },
          ]}
        />
        {viewSubject === 'cards' && viewMode === 'grid' && (
          <MemoizedCardGallery
            cards={cards}
            totalResults={totalResults}
            first={first}
            skip={skip}
            page={page}
            setSkip={setSkip}
            setFirst={setFirst}
            setPage={setPage}
            priceType={priceType}
            isFetching={isFetching}
          />
        )}
        {viewSubject === 'cards' && viewMode === 'table' && (
          <MemoizedCardTable
            cards={cards}
            totalResults={totalResults}
            first={first}
            skip={skip}
            page={page}
            setSkip={setSkip}
            setFirst={setFirst}
            setPage={setPage}
            priceType={priceType}
            isFetching={isFetching}
            isLoading={isLoading}
          />
        )}
        {viewSubject === 'sets' && viewMode === 'grid' && (
          <MemoizedSetGallery
            sets={expansions}
            costsToPurchase={costsToPurchase}
            totalResults={totalExpansionsResults}
            first={expansionsFirst}
            skip={expansionsSkip}
            page={expansionsPage}
            setSkip={setExpansionsSkip}
            setFirst={setExpansionsFirst}
            setPage={setExpansionsPage}
            priceType={priceType}
            isLoading={isLoading}
            isFetching={isFetching}
          />
        )}
        {viewSubject === 'sets' && viewMode === 'table' && (
          <MemoizedSetTable
            sets={expansions}
            totalResults={totalExpansionsResults}
            first={expansionsFirst}
            skip={expansionsSkip}
            page={expansionsPage}
            setSkip={setExpansionsSkip}
            setFirst={setExpansionsFirst}
            setPage={setExpansionsPage}
            priceType={priceType}
            isFetching={isFetching}
          />
        )}
      </ContentWrapper>
    </ResponsiveContainer>
  );
};

const MemoizedSetGallery = memo(SetGallery);
const MemoizedCardGallery = memo(CardGallery);
const MemoizedCardTable = memo(CardTable);
const MemoizedSetTable = memo(SetTable);

const ContentWrapper = styled.div(({ theme }) => ({
  marginTop: theme.spacing(0),
}));
