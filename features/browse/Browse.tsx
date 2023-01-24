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
import { useLocalStorage } from '../../util';
import { setFormVisibility } from './browseSlice';
import CardGallery from './CardGallery';
import CardTable from './CardTable';
import SetGallery from './SetGallery';
import SetTable from './SetTable';

export const Browse: React.FC = () => {
  const {
    searchQuery,
    oracleTextQuery,
    artistQuery,
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
    includeSubsets,
    includeSubsetGroups,
  } = useSelector((state: RootState) => state.browse);

  const [skip, setSkip] = useState(0);
  const [first, setFirst] = useLocalStorage('numberOfCardsPerPage', 50);
  const [page, setPage] = useState(1);

  const [expansionsSkip, setExpansionsSkip] = useState(0);
  const [expansionsFirst, setExpansionsFirst] = useLocalStorage('numberOfExpansionsPerPage', 20);
  const [expansionsPage, setExpansionsPage] = useState(1);

  const [previousTotalResults, setPreviousTotalResults] = useState(null);

  const dispatch = useDispatch();

  useEffect(() => {
    dispatch(setFormVisibility({ isFormVisibile: true }));
    return function cleanUpForm() {
      dispatch(setFormVisibility({ isFormVisibile: false }));
    };
  }, []);

  const { data: cardData, isLoading: isCardDataLoading, isFetching: isCardDataFetching, error: cardError } = useGetAllCardsQuery({
    first,
    skip,
    sortBy,
    name: searchQuery,
    oracleTextQuery,
    artistQuery,
    cardSets,
    cardRarities,
    cardTypes,
    cardColors,
    showAllPrintings,
    cardStatSearches,
    sortByDirection,
  });

  const {
    data: cardMetaData,
    isLoading: isCardMetaDataLoading,
    isFetching: isCardMetaDataFetching,
    error: cardMetaError,
  } = useGetAllCardsMetaQuery({
    sortBy,
    name: searchQuery,
    oracleTextQuery,
    artistQuery,
    cardSets,
    cardRarities,
    cardTypes,
    cardColors,
    showAllPrintings,
    cardStatSearches,
    sortByDirection,
  });

  const {
    data: costToPurchaseAll,
    isLoading: isCostsToPurchaseLoading,
    isFetching: isCostsToPurchaseFetching,
    error: costsToPurchaseError,
  } = useGetCostToPurchaseAllQuery();
  const costsToPurchase = costToPurchaseAll?.data?.costToPurchaseAll?.costToPurchaseAll;
  const cards = cardData?.data?.cards;
  const totalResults = cardMetaData?.data?.count;

  useEffect(() => {
    if (totalResults !== previousTotalResults) {
      setSkip(0);
      setPage(1);
      setPreviousTotalResults(totalResults);
    }
    if (skip > totalResults) {
      setSkip(0);
      setPage(1);
    }
  }, [skip, totalResults, previousTotalResults]);

  const { data: allSetsResponse, isLoading: isGetAllSetsLoading, isFetching: isGetAllSetsFetching } = useGetAllSetsQuery({
    first: expansionsFirst,
    skip: expansionsSkip,
    name: expansionSearchQuery,
    sortBy: sortExpansionBy,
    sortByDirection: sortExpansionByDirection,
    setTypes: expansionTypes,
    setCategories: expansionCategories,
    includeSubsets,
    includeSubsetGroups,
  });
  const expansions = allSetsResponse?.data?.sets;

  const { data: allSetsMetaResponse, isLoading: isAllSetsMetaLoading, isFetching: isAllSetsMetaFetching } = useGetAllSetsMetaQuery({
    name: expansionSearchQuery,
    sortBy: sortExpansionBy,
    sortByDirection: sortExpansionByDirection,
    setTypes: expansionTypes,
    setCategories: expansionCategories,
    includeSubsets,
    includeSubsetGroups,
  });

  const allSetsMeta = allSetsMetaResponse?.data;
  const totalExpansionsResults = allSetsMeta?.count || 0;

  const isLoading = isCardDataLoading || isCardMetaDataLoading || isCostsToPurchaseLoading || isGetAllSetsLoading || isAllSetsMetaLoading;
  const isFetching =
    isCardDataFetching || isCardMetaDataFetching || isCostsToPurchaseFetching || isGetAllSetsFetching || isAllSetsMetaFetching;

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
