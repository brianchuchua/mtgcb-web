import { useState } from 'react';
import { useLocalStorage } from '.';

interface PaginationOptions {
  initialPage: number;
  initialPageSize: number;
  localStorageKey: string;
}

export const usePagination = ({ initialPage, initialPageSize, localStorageKey }: PaginationOptions) => {
  const [skip, setSkip] = useState(0);
  const [first, setFirst] = useLocalStorage(localStorageKey, initialPageSize);
  const [page, setPage] = useState(initialPage);
  const [previousTotalResults, setPreviousTotalResults] = useState(null);

  const handleTotalResultsChange = (totalResults: number) => {
    if (totalResults !== previousTotalResults) {
      setSkip(0);
      setPage(1);
      setPreviousTotalResults(totalResults);
    }
    if (skip > totalResults) {
      setSkip(0);
      setPage(1);
    }
  };

  return { skip, setSkip, first, setFirst, page, setPage, handleTotalResultsChange };
};
