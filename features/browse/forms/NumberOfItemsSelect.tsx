import FormControl from '@material-ui/core/FormControl';
import InputLabel from '@material-ui/core/InputLabel';
import MenuItem from '@material-ui/core/MenuItem';
import Select from '@material-ui/core/Select';
import { Dispatch, SetStateAction } from 'react';

interface NumberOfItemsSelectProps {
  first: number;
  setFirst: Dispatch<SetStateAction<number>>;
  label: string;
}

const NumberOfItemsSelect: React.FC<NumberOfItemsSelectProps> = ({ first, setFirst, label }) => {
  const firstOptions = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 16, 20, 24, 25, 50, 100, 200, 300, 400, 500];

  return (
    <FormControl>
      <InputLabel id="cards-per-page">{label}</InputLabel>
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

export default NumberOfItemsSelect;
