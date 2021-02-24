import { Dispatch, SetStateAction } from 'react';
import FormControl from '@material-ui/core/FormControl';
import InputLabel from '@material-ui/core/InputLabel';
import MenuItem from '@material-ui/core/MenuItem';
import Select from '@material-ui/core/Select';

interface SearchFormProps {
  first: number;
  setFirst: Dispatch<SetStateAction<number>>;
}

const SearchForm: React.FC<SearchFormProps> = ({ first, setFirst }) => {
  const firstOptions = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 16, 25, 50, 100, 200, 300, 400, 500];

  return (
    <FormControl>
      <InputLabel id="cards-per-page">Cards</InputLabel>
      <Select labelId="cards-per-page" id="cards-per-page-select" value={first} onChange={(e) => setFirst(e.target.value as number)}>
        {firstOptions.map((option) => (
          <MenuItem key={option} value={option}>
            {option}
          </MenuItem>
        ))}
      </Select>
    </FormControl>
  );
};

export default SearchForm;
