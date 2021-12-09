import FormControl from '@material-ui/core/FormControl';
import InputAdornment from '@material-ui/core/InputAdornment';
import InputLabel from '@material-ui/core/InputLabel';
import OutlinedInput from '@material-ui/core/OutlinedInput';
import SearchIcon from '@material-ui/icons/Search';
import styled from 'styled-components';

interface CardNameSearchProps {
  searchQuery: string;
  updateSearchQuery: (query: string) => void;
}

const CardNameSearch: React.FC<CardNameSearchProps> = ({ searchQuery, updateSearchQuery }) => (
  <StyledCardNameSearch fullWidth variant="outlined">
    <InputLabel htmlFor="search-query" className="input-label-fix">
      Card Name
    </InputLabel>
    <OutlinedInput
      id="search-query"
      value={searchQuery}
      placeholder="Search by card name"
      label="Card Name"
      onChange={(e) => updateSearchQuery(e.target.value)}
      startAdornment={
        <InputAdornment position="start">
          <SearchIcon color="disabled" />
        </InputAdornment>
      }
    />
  </StyledCardNameSearch>
);

const StyledCardNameSearch = styled(FormControl)(() => ({
  paddingLeft: '8px',
  paddingRight: '8px',
  paddingBottom: '10px',
}));

export default CardNameSearch;
