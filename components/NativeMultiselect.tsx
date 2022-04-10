import TextField from '@material-ui/core/TextField';

const NativeMultiselect: React.FC<NativeMultiselectProps> = ({ label, multiselectOptions, selectedOptions, updateSelectedOptions }) => {
  const optionsByCategory = multiselectOptions.reduce((acc, type) => {
    if (!acc[type.category]) {
      acc[type.category] = [];
    }
    acc[type.category].push(type);
    return acc;
  }, {} as { [category: string]: Option[] });

  const value = selectedOptions.map((type) => type.value);

  return (
    <TextField
      select
      SelectProps={{ native: true, multiple: true, variant: 'outlined', label }}
      InputLabelProps={{
        shrink: true,
      }}
      style={{
        padding: 0,
        margin: 0,
        width: '100%',
      }}
      label={label}
      value={value}
      variant="outlined"
      onChange={(e) => {
        const { options } = (e.target as unknown) as HTMLSelectElement;
        const optionsSelected: Option[] = [];

        for (let i = 0, l = options.length; i < l; i += 1) {
          if (options[i].selected) {
            optionsSelected.push({
              category: multiselectOptions.find((option) => option.value === options[i].value).category,
              label: multiselectOptions.find((option) => option.value === options[i].value).label,
              value: options[i].value as string,
              exclude: false,
            });
          }
        }
        updateSelectedOptions(optionsSelected as Option[]);
      }}
    >
      {Object.keys(optionsByCategory).map((category) => (
        <optgroup key={category} label={category}>
          {optionsByCategory[category].map((option) => (
            <option key={option.value} value={option.value}>
              {option.label}
            </option>
          ))}
        </optgroup>
      ))}
    </TextField>
  );
};

interface Option {
  category: string;
  label: string;
  value: string;
  exclude: boolean;
}

interface NativeMultiselectProps {
  label: string;
  multiselectOptions: Option[];
  selectedOptions: Option[];
  updateSelectedOptions: (selectedOptions: Option[]) => void;
}

export default NativeMultiselect;
