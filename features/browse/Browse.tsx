import { memo, useEffect, useState } from 'react';
import { useDispatch, useSelector } from 'react-redux';
import styled from 'styled-components';
import { ResponsiveContainer } from '../../components/layout/ResponsiveContainer';
import {
  useGetAllCardsMetaQuery,
  useGetAllCardsQuery,
  useGetAllSetsMetaQuery,
  useGetAllSetsQuery,
  useGetCostToPurchaseAllQuery,
} from '../../network/services/mtgcbApi';
import { RootState } from '../../redux/rootReducer';
import useDebounce, { searchFieldDebounceTimeMs } from '../../util/useDebounce';
import { setFormVisibility } from './browseSlice';
import CardGallery from './CardGallery';
import CardTable from './CardTable';
import SetGallery from './SetGallery';
import SetTable from './SetTable';

export const Browse: React.FC = () => {
  const {
    searchQuery,
    oracleTextQuery,
    cardTypes,
    cardSets,
    cardRarities,
    cardColors,
    showAllPrintings,
    cardStatSearches,
    sortBy,
    sortByDirection,
    viewSubject,
    viewMode,
    priceType,
    expansionSearchQuery,
    sortExpansionBy,
    sortExpansionByDirection,
    expansionTypes,
    expansionCategories,
  } = useSelector((state: RootState) => state.browse);

  const debouncedSearchQuery = useDebounce(searchQuery, searchFieldDebounceTimeMs);
  const debouncedOracleTextQuery = useDebounce(oracleTextQuery, searchFieldDebounceTimeMs);
  const debouncedExpansionSearchQuery = useDebounce(expansionSearchQuery, searchFieldDebounceTimeMs);

  const [skip, setSkip] = useState(0);
  const [first, setFirst] = useState(50);
  const [page, setPage] = useState(1);

  const [expansionsSkip, setExpansionsSkip] = useState(0);
  const [expansionsFirst, setExpansionsFirst] = useState(16);
  const [expansionsPage, setExpansionsPage] = useState(1);

  const dispatch = useDispatch();

  useEffect(() => {
    dispatch(setFormVisibility({ isFormVisibile: true }));
    return function cleanUpForm() {
      dispatch(setFormVisibility({ isFormVisibile: false }));
    };
  }, []);

  const { data: cardData, isLoading: isCardDataLoading, error: cardError } = useGetAllCardsQuery({
    first,
    skip,
    sortBy,
    name: debouncedSearchQuery,
    oracleTextQuery: debouncedOracleTextQuery,
    cardSets,
    cardRarities,
    cardTypes,
    cardColors,
    showAllPrintings,
    cardStatSearches,
    sortByDirection,
  });

  const { data: cardMetaData, isLoading: isCardMetaDataLoading, error: cardMetaError } = useGetAllCardsMetaQuery({
    sortBy,
    name: debouncedSearchQuery,
    oracleTextQuery: debouncedOracleTextQuery,
    cardSets,
    cardRarities,
    cardTypes,
    cardColors,
    showAllPrintings,
    cardStatSearches,
    sortByDirection,
  });

  const { data: costToPurchaseAll, isLoading: isCostsToPurchaseLoading, error: costsToPurchaseError } = useGetCostToPurchaseAllQuery();
  const costsToPurchase = costToPurchaseAll?.data?.costToPurchaseAll?.costToPurchaseAll;
  const cards = cardData?.data?.allCards;
  const totalResults = cardMetaData?.data?._allCardsMeta?.count;

  useEffect(() => {
    if (skip > totalResults) {
      setSkip(0);
      setPage(1);
    }
  }, [skip, totalResults]);

  const { data: allSetsResponse } = useGetAllSetsQuery({
    first: expansionsFirst,
    skip: expansionsSkip,
    name: debouncedExpansionSearchQuery,
    sortBy: sortExpansionBy,
    sortByDirection: sortExpansionByDirection,
    setTypes: expansionTypes,
    setCategories: expansionCategories,
  });
  const expansions = allSetsResponse?.data?.allSets;

  const { data: allSetsMetaResponse } = useGetAllSetsMetaQuery({
    name: debouncedExpansionSearchQuery,
    sortBy: sortExpansionBy,
    sortByDirection: sortExpansionByDirection,
    setTypes: expansionTypes,
    setCategories: expansionCategories,
  });
  const allSetsMeta = allSetsMetaResponse?.data?._allSetsMeta;
  const totalExpansionsResults = allSetsMeta?.count || 0;

  useEffect(() => {
    if (expansionsSkip > totalExpansionsResults) {
      setExpansionsSkip(0);
      setExpansionsPage(1);
    }
  }, [expansionsSkip, totalExpansionsResults]);

  return (
    <ResponsiveContainer maxWidth="xl">
      <ContentWrapper>
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
