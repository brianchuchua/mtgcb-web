import MuiPagination from '@material-ui/lab/Pagination';
import { Dispatch, SetStateAction } from 'react';
import styled from 'styled-components';

interface PaginationProps {
  total: number;
  page: number;
  first: number;
  setPage: Dispatch<SetStateAction<number>>;
  setSkip: Dispatch<SetStateAction<number>>;
}

const Pagination: React.FC<PaginationProps> = ({ total, page, first, setPage, setSkip }) => {
  const count = Math.ceil(total / first);

  return (
    <StyledPagination
      count={count}
      page={page}
      onChange={(event: React.ChangeEvent<unknown>, value: number) => {
        setPage(value);
        setSkip((value - 1) * first);
      }}
    />
  );
};

const StyledPagination = styled(MuiPagination)({
  textAlign: 'center',
  display: 'inline-block',
});

export default Pagination;
